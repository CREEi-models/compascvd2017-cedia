! COMPAS model: main code

module compas
	! for openmp
    use omp_lib
   	use statamod

		use random
    ! declaring variables for main program
    implicit none

    ! define a person and it attributes
    include 'person.f95'
    ! include settings
    include 'settings.f95'
		!include the path of stata determine in compile_linux.sh
		include 'stata_path.f95'
    !define a statistic
    include 'statistic.f95'
    ! defines an intervention
    include 'intervention.f95'
    ! sets name of parameters for transition model
    include 'transit.f95'
    ! sets the list of derived vars that will be used
    include 'derivars.f95'
  	! sets the name of parameters for new cohorts models
  	include 'newcohorts.f95'
  	! sets name of parameters for trends
  	include 'trends.f95'
  	! sets names of parameters for health care
  	include 'hcare.f95'

	contains
	   ! general settings of scenario
       subroutine loadsettings
            character*80 buffer
            character*20 switch, type
            character*80 nameofintervention
            integer narg, nprov
            integer i,j
            logical flag
            integer status
						integer size_seed
						real, allocatable :: draw(:,:)

            ! check for arguments at execution (name of scenario)
            narg = command_argument_count()
            call get_command_argument(1,scenario)

            write(*,*) '**** Simulation using COMPAS ****'
            if (scenario .eq. ' ') then
                write(*,*) 'ERROR: no scenario defined'
                stop
            end if

            ! create the output directory if does not exist
            call getpath(path)

            outpath =  adjustl(trim(path))//'output/'//adjustl(trim(scenario))
            call system('mkdir -p '//adjustl(trim(outpath)))
            call system('mkdir -p '//adjustl(trim(outpath))//'/settings')

            ! load settings file for scenario
            inquire(file=adjustl(trim(path))//'settings/settings-'//adjustl(trim(scenario))//'.dat', exist=flag)



            if (flag) then
               call system('cp '//adjustl(trim(path))//'settings/settings-'//adjustl(trim(scenario))//'.dat ' &
                //adjustl(trim(outpath))//'/settings/settings-'//adjustl(trim(scenario))//'.dat')
                open(1, file=adjustl(trim(path))//'settings/settings-'//adjustl(trim(scenario))//'.dat')
                    read(1,*) buffer, startyear
                    read(1,*) buffer, stopyear
                    read(1,*) buffer, stopyear_enter
                    read(1,*) buffer, gapyears
										read(1,*) buffer, endstats
                    read(1,*) buffer, startage
                    read(1,*) buffer, stopage
                    read(1,*) buffer, doreps
                    read(1,*) buffer, nreps
                    read(1,*) buffer, immigration
                    read(1,*) buffer, newcohort
                    read(1,*) buffer, imortality
                    read(1,*) buffer, ihcarecost
                    read(1,*) buffer, ieducation
                    read(1,*) buffer, ismoke
                    read(1,*) buffer, ibmi
                    read(1,*) buffer, iquebec
										read(1,*) buffer, updhcare
										read(1,*) buffer, exportstata_opt, start_byear, end_byear
										read(1,*) buffer, educ_intervention
                    read(1,*) buffer, ncoef
                    read(1,*) buffer, decrcvd,alpha
                    read(1,*) buffer, decrmortality


                close(1)
            else
                write(*,*) 'WARNING: could not find settings file, copying reference.dat'
                call system('cp '//adjustl(trim(path))//'settings/settings-reference.dat '&
                	//adjustl(trim(path))//'settings/settings-' &
                    //adjustl(trim(scenario))//'.dat')
               call system('cp  '//adjustl(trim(path))//'settings/settings-'//adjustl(trim(scenario))//'.dat ' &
                             //adjustl(trim(outpath))//'/settings/settings-'//adjustl(trim(scenario))//'.dat')
                open(1, file=adjustl(trim(path))//'settings/settings-'//adjustl(trim(scenario))//'.dat')
                    read(1,*) buffer, startyear
                    read(1,*) buffer, stopyear
                    read(1,*) buffer, stopyear_enter
                    read(1,*) buffer, gapyears
										read(1,*) buffer, endstats
                    read(1,*) buffer, startage
                    read(1,*) buffer, stopage
                    read(1,*) buffer, doreps
                    read(1,*) buffer, nreps
                    read(1,*) buffer, newcohort
                    read(1,*) buffer, immigration
                    read(1,*) buffer, imortality
                    read(1,*) buffer, ihcarecost
                    read(1,*) buffer, ieducation
                    read(1,*) buffer, ismoke
                    read(1,*) buffer, ibmi
                    read(1,*) buffer, iquebec
										read(1,*) buffer, updhcare
										read(1,*) buffer, exportstata_opt, start_byear, end_byear
										read(1,*) buffer, educ_intervention
										read(1,*) buffer, ncoef
                    read(1,*) buffer, decrcvd
                    read(1,*) buffer, decrmortality

                close(1)
            end if

            ! compute other parameters needed
            ncycle = 1 + (stopyear_enter - startyear)/gapyears
            nyears = (stopyear - startyear)/gapyears + 1
            nages  = stopage - startage + 1
						nyearstats = (endstats- startyear)/gapyears + 1
            write(*,*) 'running scenario : ', trim(adjustl(scenario))
            write(*,*) 'in population mode ...'
            if (iquebec) then
               write(*,*) ' - for Quebec only'
            else
               write(*,*) ' - for Canada '
            end if
            !Load statistics list
            call system('cp  '//adjustl(trim(path))//'params/stats/statslist.dat ' &
                             //adjustl(trim(outpath))//'/settings/'//'statslist.dat')
            open(1,file=adjustl(trim(path))//'params/stats/statslist.dat')
            read(1,*) nstats
            allocate(stats(nstats))
            do i = 1, nstats, 1
                read(1,*) stats(i)%outname, stats(i)%cmd, stats(i)%varname, &
                    stats(i)%byvarname, stats(i)%gap,stats(i)%byvarname2, stats(i)%gap2, stats(i)%wgt
                !byvar
                if (stats(i)%byvarname .eq. 'age') then
                    stats(i)%nbyvar = int(floor(dble(stopage - startage)/dble(stats(i)%gap))) + 1
                    stats(i)%maxbyvar = stopage
                    stats(i)%minbyvar = startage
                else if (stats(i)%byvarname .eq. 'byear') then
                    stats(i)%nbyvar = int(floor(dble(stopage - startage)/dble(stats(i)%gap))) + 1 !????
                    stats(i)%maxbyvar = startyear - stopage
                    stats(i)%minbyvar = startyear
                else if  (stats(i)%byvarname .eq. 'province') then
                    if (iquebec .eqv. .TRUE.) then
                    stats(i)%nbyvar = 1
                    stats(i)%maxbyvar = 2
                    stats(i)%minbyvar = 2
                    else
                    stats(i)%nbyvar = 5
                    stats(i)%maxbyvar = 5
                    stats(i)%minbyvar = 1
                    end if
                else if  (stats(i)%byvarname .eq. 'year') then
                    stats(i)%nbyvar = 1
                    stats(i)%maxbyvar = 1
                    stats(i)%minbyvar = 1
                else if  (stats(i)%byvarname .eq. 'educ') then
                    stats(i)%nbyvar = 4
                    stats(i)%maxbyvar = 4
                    stats(i)%minbyvar = 1
                else if  (stats(i)%byvarname .eq. 'inv') then
                    stats(i)%nbyvar = 7
                    stats(i)%maxbyvar = 7
                    stats(i)%minbyvar = 1
                else if  (stats(i)%byvarname .eq. 'ltc') then
                    stats(i)%nbyvar = 3
                    stats(i)%maxbyvar = 3
                    stats(i)%minbyvar = 1
                else if  (stats(i)%byvarname .eq. 'hours_homecare_f') then
                    stats(i)%nbyvar = 5000
                    stats(i)%maxbyvar = 5000
                    stats(i)%minbyvar = 1
								else if  (stats(i)%byvarname .eq. 'hc_nights') then
											stats(i)%nbyvar = 366
											stats(i)%maxbyvar = 365
											stats(i)%minbyvar = 0
								else if  (stats(i)%byvarname .eq. 'hc_specialist') then
													stats(i)%nbyvar = 365
													stats(i)%maxbyvar = 365
													stats(i)%minbyvar = 0
								else if  (stats(i)%byvarname .eq. 'hc_generalist') then
															stats(i)%nbyvar = 365
															stats(i)%maxbyvar = 365
															stats(i)%minbyvar = 0
                else
                    stats(i)%nbyvar = 2
                    stats(i)%maxbyvar = 1
                    stats(i)%minbyvar = 0
                end if

                !byvar2
                if (stats(i)%byvarname2 .eq. 'NULL') then
                    stats(i)%nbyvar2 = 1
                    stats(i)%maxbyvar2 = 1
                    stats(i)%minbyvar2 = 1
                else if (stats(i)%byvarname2 .eq. 'age') then
                    stats(i)%nbyvar2 = int(floor(dble(stopage - startage)/dble(stats(i)%gap))) + 1
                    stats(i)%maxbyvar2 = stopage
                    stats(i)%minbyvar2 = startage
                else if (stats(i)%byvarname2 .eq. 'byear') then
                    stats(i)%nbyvar2 = int(floor(dble(stopage - startage)/dble(stats(i)%gap))) + 1 !????
                    stats(i)%maxbyvar2 = startyear - stopage
                    stats(i)%minbyvar2 = startyear
                else if  (stats(i)%byvarname2 .eq. 'province') then
                    if (iquebec .eqv. .TRUE.) then
                        stats(i)%nbyvar2 = 1
                        stats(i)%maxbyvar2 = 2
                        stats(i)%minbyvar2 = 2
                    else
                        stats(i)%nbyvar2 = 5
                        stats(i)%maxbyvar2 = 5
                        stats(i)%minbyvar2 = 1
                    end if
                else if  (stats(i)%byvarname2 .eq. 'year') then
                    stats(i)%nbyvar2 = 1
                    stats(i)%maxbyvar2 = 1
                    stats(i)%minbyvar2 = 1
                else if  (stats(i)%byvarname2 .eq. 'educ') then
                    stats(i)%nbyvar2 = 4
                    stats(i)%maxbyvar2 = 4
                    stats(i)%minbyvar2 = 1
                else if  (stats(i)%byvarname2 .eq. 'inv') then
                    stats(i)%nbyvar2 = 7
                    stats(i)%maxbyvar2 = 7
                    stats(i)%minbyvar2 = 1
                else if  (stats(i)%byvarname2 .eq. 'ltc') then
                    stats(i)%nbyvar2 = 3
                    stats(i)%maxbyvar2 = 3
                    stats(i)%minbyvar2 = 1

                else if  (stats(i)%byvarname2 .eq. 'hours_homecare_f') then
                    stats(i)%nbyvar2 = 5000
                    stats(i)%maxbyvar2 = 5000
                    stats(i)%minbyvar2 = 1
								else if  (stats(i)%byvarname .eq. 'hc_nights') then
												stats(i)%nbyvar = 366
												stats(i)%maxbyvar = 365
												stats(i)%minbyvar = 0

                else
                    stats(i)%nbyvar2 = 2
                    stats(i)%maxbyvar2 = 1
                    stats(i)%minbyvar2 = 0
                end if
                allocate(stats(i)%value(nyears,stats(i)%nbyvar,stats(i)%nbyvar2))
                allocate(stats(i)%sd(nyears,stats(i)%nbyvar,stats(i)%nbyvar2))
                stats(i)%value(:,:,:) = 0.0d0
                stats(i)%sd(:,:,:) = 0.0d0
            end do

            ! if scenario other than reference, load interventions and restrictions
            if (scenario .ne. 'reference') then
                call system('cp  '//adjustl(trim(path))//'scenarios/'//adjustl(trim(scenario))//'.dat ' &
                             //adjustl(trim(outpath))//'/settings/'//adjustl(trim(scenario))//'.dat')
                open(1, file=adjustl(trim(path))//'scenarios/'//adjustl(trim(scenario))//'.dat')
                read(1,*) ninterventions
                if (ninterventions.gt.0) then
                    allocate(interventions(ninterventions))
                    write(*,*) 'scenario has ', ninterventions, ' intervention(s)'

               		do i = 1, ninterventions, 1
                    	read(1,*) nameofintervention
                    	call system('cp  '//adjustl(trim(path))//'interventions/'//adjustl(trim(nameofintervention))//'.dat ' &
                             //adjustl(trim(outpath))//'/settings/'//adjustl(trim(nameofintervention))//'.dat')
                    	open(2, file=adjustl(trim(path))//'interventions/'//adjustl(trim(nameofintervention))//'.dat')
                    	read(2,*) interventions(i)%disease
                    	read(2,*) interventions(i)%prevention
                    	read(2,*) interventions(i)%prevention_prob
                    	read(2,*) interventions(i)%cure
                    	read(2,*) interventions(i)%cure_prob
                    	close(2)
                    	write(*,*) interventions(i)
                	end do
                else
                    write(*,*) 'WARNING: scenario does not contain an intervention'
                end if

                read(1,*,iostat=status) nrestrictions
                if (status .ne. 0) then
                    nrestrictions = 0
                    close(1)
                else
                    allocate(restrictions(nrestrictions))
                    write(*,*) 'scenario has ', nrestrictions, ' restriction(s) on transit params'
                    do i = 1, nrestrictions, 1
                        read(1,*) restrictions(i)%disease, restrictions(i)%eq,  &
                            restrictions(i)%var, restrictions(i)%val
                    end do
                    close(1)
                end if

            end if

            ! load derived var labels
            open(1,file=adjustl(trim(path))//'settings/derivedvars.dat')
            read(1,*) nderivedvars
            allocate(derivedvarlabels(nderivedvars))
            do i = 1, nderivedvars, 1
            	read(1,*) derivedvarlabels(i)
            end do
            close(1)


						! load trends
						call loadtrends

            ! seed
            call random_seed(size=size_seed)
            allocate(seed(size_seed))
            seed(:) =12345678
            call random_seed(put=seed)
						allocate(seed_cmp(nreps*ncycle,size_seed))
						allocate(draw(nreps*ncycle*size_seed,2))
						call random_number(draw(:,1))
						call random_number(draw(:,2))

						do i =1, nreps*ncycle
							do j =1,size_seed
								seed_cmp(i,j) = int(draw(i+(size_seed*(j-1)),1)*draw(i+(size_seed*(j-1)),2)*100000000d+0)
							end do
						end do
						deallocate(seed)

        end subroutine loadsettings

        subroutine loadtransitparams(transit_models,ntransit,type,coef)
            type (model), allocatable :: transit_models(:)
            integer i, npar, j, k,l, s, age, sex, flag, ntransit, dim_param,coef
            character*80 buffer
            character*20 type
						character(len=4) :: cr
						write(cr,"(i4)") coef
						write(*,*) "parameter set : ", cr
            ! load model names and parameters
            open(1, file=adjustl(trim(path))//'params/'//adjustl(trim(type))//'/settings.dat')
            read(1,*) ntransit
            allocate(transit_models(ntransit))
            do i = 1, ntransit, 1
            	read(1,*) transit_models(i)%name, transit_models(i)%spec, transit_models(i)%neq, transit_models(i)%nsex
            	open(2,file=adjustl(trim(path))//'params/'//adjustl(trim(type))//'/'//trim(adjustl(transit_models(i)%name))&
              //trim(adjustl(cr))//'.csv')
            		dim_param = transit_models(i)%neq * transit_models(i)%nsex
            		 allocate(transit_models(i)%param(dim_param))

                do j = 1, dim_param, 1
                    read(2,*) transit_models(i)%param(j)%npar

                    allocate(transit_models(i)%param(j)%val(transit_models(i)%param(j)%npar))
                    allocate(transit_models(i)%param(j)%var(transit_models(i)%param(j)%npar))
                    do k = 1,transit_models(i)%param(j)%npar, 1
                        read(2,*) transit_models(i)%param(j)%var(k),transit_models(i)%param(j)%val(k)
                        ! check that varname exist
                        if (k .eq. 1) then
                            flag =1
                            do l = 1, nderivedvars, 1
                                if (transit_models(i)%param(1)%var(k) .eq. derivedvarlabels(l)) then
                                 flag = 0
                                end if
                            end do
                            if (flag .eq. 1) then
                                write(*,*) 'WARNING: variable ',adjustl(trim(transit_models(i)%param(1)%var(k))), &
                                    ' of model ',adjustl(trim(transit_models(i)%name)), ' not in derived var list'
                            end if
                        end if

                    end do
                end do

                if (transit_models(i)%spec .eq. negbin) then
                    read(2,*) transit_models(i)%dispersion
										transit_models(i)%dispersion =exp(transit_models(i)%dispersion)
                end if
				close(2)
				if (nrestrictions .ge. 1) then
					do s = 1, nrestrictions, 1
						if (restrictions(s)%disease .eq. transit_models(i)%name) then
							do k = 1, transit_models(i)%neq, 1
								if (restrictions(s)%eq .eq. k) then
									do j = 1, transit_models(i)%param(k)%npar, 1
										if (transit_models(i)%param(k)%var(j) .eq. restrictions(s)%var) then
											transit_models(i)%param(k)%val(j) = restrictions(i)%val
										end if
									end do
								end if
							end do
						end if
					end do
				end if
			end do
            close(1)
            ! load adjustments factor for mortality (file creates up to age 100)
            if (.not. allocated(calibmortality)) then
                open(1, file=adjustl(trim(path))//'params/transit/factor'//trim(adjustl(cr))//'.csv')
                allocate(calibmortality(nages,2))
                do i=1, 100-30, 1
                    read(1,*) sex, age, calibmortality(i,1)
                end do
                do i = 101-30, nages
                    calibmortality(i,1) = calibmortality(100-30,1)
                end do
                do i=1, 100-30, 1
                    read(1,*) sex, age, calibmortality(i,2)
                end do
                do i = 101-30, nages
                    calibmortality(i,2) = calibmortality(100-30,2)
                end do
                close(1)
            end if
        end subroutine loadtransitparams

        subroutine loadnewcohortparams(rep)
        	integer i, ntot, ii,rep
        	type (person), allocatable :: tempop(:)
					character(len=4) :: cr
					character*20 buffer
					rep = 1+modulo(rep-1,100)
					write(cr,"(i4)") rep
					write(*,*) "newcohort rep : ", cr
        	open(1,file=adjustl(trim(path))//'params/newcohorts/nobs'//trim(adjustl(cr))//'.csv')
        	read(1,*) ntot
        	close(1)
        	allocate(tempop(ntot))
        	open(1,file=adjustl(trim(path))//'data/input/newcohorts'//trim(adjustl(cr))//'.csv')
        	ncohortsize = 0
        	do i = 1, ntot, 1
        		read(1,*) tempop(i)%id,tempop(i)%age,tempop(i)%byear,tempop(i)%educ, tempop(i)%income, &
        			tempop(i)%sex, tempop(i)%imm, tempop(i)%province, tempop(i)%diabe, tempop(i)%hibpe, &
        			tempop(i)%hearte, tempop(i)%stroke, tempop(i)%cancre, tempop(i)%lunge, &
        			tempop(i)%mentae, tempop(i)%smoke, tempop(i)%bmi, tempop(i)%inv, tempop(i)%ltc, tempop(i)%wgt
              tempop(i)%idiabe = 0
              tempop(i)%ihibpe = 0
        			tempop(i)%ihearte = 0
              tempop(i)%istroke = 0
              tempop(i)%icancre =0
              tempop(i)%ilunge = 0
        			tempop(i)%imentae = 0
              tempop(i)%init = 1
	        		tempop(i)%alive = 1
	        		tempop(i)%hc_nights = 0
	        		tempop(i)%hc_specialist = 0
	        		tempop(i)%hc_generalist = 0
	        		tempop(i)%hc_homecare_f = 0
	        		tempop(i)%hc_homecare_i = 0
	        		tempop(i)%hc_homecare_sp = 0
	        		tempop(i)%hc_drugs = 0
	        		tempop(i)%hc_hui = 0.0d0
	        		tempop(i)%nyears = -1
	            tempop(i)%nyears_disable = -1
	            tempop(i)%nyears_disease = -1
	            tempop(i)%nyears_nhome = 0
	            tempop(i)%ydisable = -1
	            tempop(i)%ydisease = -1
	        		tempop(i)%cost_nights = 0.0d0
	        		tempop(i)%cost_specialist = 0.0d0
	        		tempop(i)%cost_generalist = 0.0d0
	        		tempop(i)%cost_homecare_f = 0.0d0
	        		tempop(i)%hours_homecare_f = 0.0d0
	        		tempop(i)%hours_homecare_i = 0.0d0
	        		tempop(i)%hours_homecare_sp = 0.0d0
	        		tempop(i)%cost_drugs = 0.0d0
	        		tempop(i)%cost_nhome = 0.0d0
	        		tempop(i)%cost_nhome = 0.0d0
							tempop(i)%life_cost_nhome = 0.0d0
	        		tempop(i)%life_disc_cost_nhome = 0.0d0
							!Count only quebec individual for cohort size if iquebec=true
                if (iquebec) then
                   if (tempop(i)%province .eq. 2) then
                      ncohortsize = ncohortsize + 1
                   end if
                else
                   ncohortsize = ncohortsize + 1
                end if
        	end do
        	close(1)
        	allocate(newpop(ncohortsize))
        	ii = 1
            do i = 1, ntot, 1
            if (iquebec) then
        		if (tempop(i)%province .eq. 2) then
        			newpop(ii) = tempop(i)
        			ii = ii + 1
                 end if
            else
               newpop(ii) = tempop(i)
               ii = ii + 1
            end if
        	end do

        	deallocate(tempop)

        	! input parameters of new cohorts models
        	open(1,file=adjustl(trim(path))//'params/newcohorts/models.dat')
        	read(1,*) ncohortmodels
        	allocate(newcohort_models(ncohortmodels))
        	do i = 1, ncohortmodels, 1
        		read(1,*) newcohort_models(i)%label, newcohort_models(i)%spec, &
        			newcohort_models(i)%nvalues
        		open(2,file=adjustl(trim(path))//'params/newcohorts/mean_'&
        			//adjustl(trim(newcohort_models(i)%label))//trim(adjustl(cr))//'.csv')
        		read(2,*) newcohort_models(i)%npar
        		allocate(newcohort_models(i)%param%var(newcohort_models(i)%npar))
        		allocate(newcohort_models(i)%param%val(newcohort_models(i)%npar))
        		do ii = 1,newcohort_models(i)%npar, 1
        			read(2,*) newcohort_models(i)%param%var(ii),newcohort_models(i)%param%val(ii)
        		end do
        		close(2)
        		if (newcohort_models(i)%spec .eq. oprobit) then
        			allocate(newcohort_models(i)%thres%val(newcohort_models(i)%nvalues + 1))
        			open(2,file=adjustl(trim(path))//'params/newcohorts/thres_'&
        				//adjustl(trim(newcohort_models(i)%label))//trim(adjustl(cr))//'.csv')
        				do ii = 1, newcohort_models(i)%nvalues+1, 1
        					read(2,*) newcohort_models(i)%thres%val(ii)
        				end do
        			close(2)
        		end if
        	end do
        	close(1)

        	allocate(newcohort_choleski(ncohortmodels,ncohortmodels))
        	open(1,file=adjustl(trim(path))//'params/newcohorts/choleski'//trim(adjustl(cr))//'.csv')
        	do i = 1, ncohortmodels, 1
				read(1,*) newcohort_choleski(i,:)
        	end do
        	close(1)

					! load adjustments factor for mortality (file creates up to age 100)
					if (.not. allocated(calibeduc)) then
								allocate(calibeduc(4,5,2))
					end if
					open(1, file=adjustl(trim(path))//'params/newcohorts/factor_educ'//trim(adjustl(cr))//'.csv')

							do i=1, 5, 1
									read(1,*) buffer, buffer, calibeduc(:,i,1)
							end do
							do i=1, 5, 1
									read(1,*) buffer, buffer, calibeduc(:,i,2)
							end do
					close(1)

        end subroutine loadnewcohortparams



        ! trends in model
        subroutine loadtrends
        	integer i, p, s, t, year,nages_min
        	character*1 is
        	character*1 ip
        	character*120 filename
					nages_min = min(nages,110-30)
        	! mortality trends
        	allocate(mortalitytrend(5, 2, nyears, nages))
        	do p = 1, 5, 1
        		write(ip,'(i1)') p
        		do s =1 , 2, 1
        			if (imortality) then
        			write(is,'(i1)') s-1
        			filename = adjustl(trim(path))//'params/trends/trend-mortality-'//ip//'-'//is//'.csv'
        			open(1,file=adjustl(trim(filename)))
        			do t = 1, ncycle, 1
        				read(1,*)  year, mortalitytrend(p,s,t,1:nages_min)
								if (nages .gt. nages_min) then
									mortalitytrend(p,s,t,nages_min+1:nages) = 1
								end if
        			end do
        			do t = ncycle+1, nyears, 1
        				mortalitytrend(p,s,t,:) = mortalitytrend(p,s,ncycle,:)
        			end do
        			close(1)
        			else
        				mortalitytrend(p,s,:,:) = 1.0d0
        			end if
        		end do
        	end do

        	! health care cost trends
        	allocate(trendhcare(nyears))
        	if (ihcarecost) then
        	open(1,file=adjustl(trim(path))//'params/trends/trend_hcare.csv')
        		do i = 1, ncycle, 1
        			read(1,*) year, trendhcare(i)%nights, trendhcare(i)%specialist, &
        				trendhcare(i)%generalist, trendhcare(i)%drugs, trendhcare(i)%homecare, &
        				trendhcare(i)%nhome
        		end do
        		do i = ncycle+1, nyears, 1
        			trendhcare(i) = trendhcare(ncycle)
        		end do

        	close(1)
        	else
        		trendhcare(:)%nights = 1.0d0
        		trendhcare(:)%specialist = 1.0d0
        		trendhcare(:)%generalist = 1.0d0
        		trendhcare(:)%drugs = 1.0d0
        		trendhcare(:)%homecare = 1.0d0
        		trendhcare(:)%nhome = 1.0d0
        	end if
        	! age 30-31 population size (relative, 2010 = 1)
        	allocate(pop30trend(2,ncycle,5))
        	open(1,file=adjustl(trim(path))//'params/trends/trend-pop30_male.csv')
        	do i = 1, ncycle, 1
        		read(1,*) year, pop30trend(1,i,:)
        	end do
 			close(1)
        	open(1,file=adjustl(trim(path))//'params/trends/trend-pop30_female.csv')
        	do i = 1, ncycle, 1
        		read(1,*) year, pop30trend(2,i,:)
        	end do
 			close(1)
         	! immigration trends
         	nage_imm = 3
        	allocate(immtrend(5, 2, nyears, nage_imm))
        	do p = 1, 5, 1
        		write(ip,'(i1)') p
        		do s =1 , 2, 1
        			write(is,'(i1)') s-1
        			if (immigration) then
						filename = adjustl(trim(path))//'params/trends/trend-immigration-'//ip//'-'//is//'.csv'
						open(1,file=adjustl(trim(filename)))
						do t = 1, ncycle, 1
							read(1,*)  year, immtrend(p,s,t,:)
						end do
						do t = ncycle+1, nyears, 1
							immtrend(p,s,t,:) = 0.0d0
						end do
						close(1)

        			else
        				immtrend(p,s,:,:) = 0.0d0
        			end if
        		end do
        	end do
        	! new cohort trends
        	allocate(trendnew(nyears))
        	! education
        	if (ieducation) then
        		open(1,file=adjustl(trim(path))//'params/trends/trend_educ.csv')
        		do i = 1, ncycle, 1
        			read(1,*) year, trendnew(i)%educ(:)
        		end do
        		close(1)
        	else
        		do i = 1, ncycle, 1
        			trendnew(i)%educ(:) = 1.0d0
        		end do
        	end if
        	do i = ncycle + 1, nyears, 1
        		trendnew(i)%educ(:) = 1.0d0
        	end do
        	! bmi
        	if (ibmi) then
        		open(1,file=adjustl(trim(path))//'params/trends/trend_bmi.csv')
        		do i = 1, ncycle, 1
        			read(1,*) year, trendnew(i)%bmi(:)
        		end do
        		close(1)
        	else
        		do i = 1, ncycle, 1
        			trendnew(i)%bmi(:) = 1.0d0
        		end do
        	end if
        	do i = ncycle + 1, nyears, 1
        		trendnew(i)%bmi(:) = 1.0d0
        	end do
        	! smoke
        	if (ismoke) then
        		open(1,file=adjustl(trim(path))//'params/trends/trend_smoke.csv')
        		do i = 1, ncycle, 1
        			read(1,*) year, trendnew(i)%smoke(:)
        		end do
        		close(1)
        	else
        		do i = 1, ncycle, 1
        			trendnew(i)%smoke(:) = 1.0d0
        		end do
        	end if

        	do i = ncycle + 1, nyears, 1
        		trendnew(i)%smoke(:) = 1.0d0
        	end do



        end subroutine loadtrends

        ! load initial population
        subroutine getinitialpop(pop, npop,rep)
        	type (person), allocatable :: tempop(:), pop(:)
        	integer i, npop, ntempop, ii,rep
					character(len=4) :: cr
					rep = 1+modulo(rep-1,100)
					write(cr,"(i4)") rep
 					write(*,*)"Initial cohort: ", cr
        	open(1,file=adjustl(trim(path))//'data/input/sizeofstartpop'//trim(adjustl(cr))//'.csv')
        	read(1,*) ntempop
        	close(1)
        	allocate(tempop(ntempop))
        	open(1,file=adjustl(trim(path))//'data/input/startpop'//trim(adjustl(cr))//'.csv')
        	npop = 0
        	nsimcases = ntempop
        	do i = 1, ntempop, 1
        		read(1,*) tempop(i)%id,tempop(i)%age,tempop(i)%byear,tempop(i)%educ, &
        			tempop(i)%sex, tempop(i)%imm, tempop(i)%province, tempop(i)%diabe, tempop(i)%hibpe, &
        			tempop(i)%hearte, tempop(i)%stroke, tempop(i)%cancre, tempop(i)%lunge, &
        			tempop(i)%mentae, tempop(i)%smoke, tempop(i)%bmi, tempop(i)%inv, tempop(i)%income,tempop(i)%ltc, tempop(i)%wgt
        		! initialize other variables
            tempop(i)%idiabe = 0
            tempop(i)%ihibpe = 0
            tempop(i)%ihearte = 0
            tempop(i)%istroke = 0
            tempop(i)%icancre =0
            tempop(i)%ilunge = 0
            tempop(i)%imentae = 0
            tempop(i)%alive = 1
                tempop(i)%init = 1
        		tempop(i)%year = startyear
        		tempop(i)%hc_nights = 0
        		tempop(i)%hc_specialist = 0
        		tempop(i)%hc_generalist = 0
        		tempop(i)%hc_homecare_f = 0
        		tempop(i)%hc_homecare_i = 0
        		tempop(i)%hc_homecare_sp = 0
        		tempop(i)%hc_drugs = 0
        		tempop(i)%hc_hui = 0
        		tempop(i)%nyears = -1
                tempop(i)%nyears_disable = -1
                tempop(i)%nyears_disease = -1
                tempop(i)%nyears_nhome = 0
                tempop(i)%ydisable = -1
                tempop(i)%ydisease = -1
                tempop(i)%nyears_nhome = -1
        		tempop(i)%cost_nights = 0.0d0
        		tempop(i)%cost_specialist = 0.0d0
        		tempop(i)%cost_generalist = 0.0d0
        		tempop(i)%cost_homecare_f = 0.0d0
        		tempop(i)%hours_homecare_f = 0.0d0
        		tempop(i)%hours_homecare_i = 0.0d0
        		tempop(i)%hours_homecare_sp = 0.0d0
        		tempop(i)%cost_drugs = 0.0d0
                tempop(i)%cost_nhome = 0.0d0
                tempop(i)%life_cost_nhome = 0.0d0
        		tempop(i)%life_disc_cost_nhome = 0.0d0
                if (iquebec) then
        		    if (tempop(i)%province .eq. 2) then
        			   npop = npop + 1
                    end if
               else
                  npop = npop + 1
               end if
        		tempop(i)%id = i
        	end do
        	close(1)
        	allocate(pop(npop))
        	ii = 1
            do i = 1, ntempop, 1

                if (iquebec) then
        		   if (tempop(i)%province .eq. 2) then
        			   pop(ii) = tempop(i)
        			   pop(ii)%id = ii
        			   ii = ii + 1
                end if
                else
                   pop(ii) = tempop(i)
                   ii = ii + 1
                end if
        	end do
        	deallocate(tempop)

        end subroutine getinitialpop

        ! main function to execute model
        subroutine runpop
           	integer i,j,k, nstartpop,npoptot,coef_number
        		type (person), allocatable :: init_pop(:)
            character*20 type

        	! initialize starting population

        	! do computations
					do k  = 1, ncoef,1
						! load transition parameters
						type = 'transit'
						call loadtransitparams(transit_models,ntransit,type,k)



						! load health care parameters
						type =  'hcare'
						call loadtransitparams(hcare_models,nhcare,type,1)
						do j = 1, nreps, 1
								call getinitialpop(init_pop, nstartpop,j)

								if (newcohort) then
									call loadnewcohortparams(j)
									npoptot = nstartpop+ncohortsize*ncycle
								else
									npoptot = nstartpop
								end if

								call runonepopreps(j,init_pop, nstartpop,npoptot)
								deallocate(init_pop)

								if (newcohort) then
									deallocate(newpop)
									deallocate(newcohort_models)
									deallocate(newcohort_choleski)
								end if

	          end do

	            call exportstats(k)
							deallocate(calibmortality)
							deallocate(transit_models)
              deallocate(hcare_models)

              do i = 1, nstats, 1
                stats(i)%value(:,:,:) = 0.0d0
              end do
					end do
	            !call changeflag
        end subroutine runpop

        ! do one replication
        subroutine runonepopreps(r,init_pop,nstartpop,npoptot)
        	integer r, nstartpop, t, npop, ii, i, year,test,rnyears,nobs,j, nextid,npoptot,testt
        	integer nsurv_pop, nnewpop, provpos,sexpos
        	double precision start_time, stop_time
        	type (person) init_pop(nstartpop)
        	character*1 sj
        	character(len=4) :: cr
        	type (person), allocatable :: currentpop(:)
        	type (person), allocatable :: nextpop(:)
        	type (person), allocatable :: allpersons(:)
        	type (person) thisnewpop(ncohortsize)
        	type (totpop) popallyear(nyears)
        	type (person) allpopulation(nyears,npoptot)

        	integer allalive(npoptot)

        	write(*,*) 'replication ', r
        	allpopulation(:,:)%init = 0
        	allpopulation(:,:)%alive = 0
        	allpopulation(:,:)%nyears = -1
            allpopulation(:,:)%nyears_disable = -1
            allpopulation(:,:)%nyears_disease = -1
            allpopulation(:,:)%nyears_nhome = 0
            allpopulation(:,:)%life_cost_nhome = 0
            allpopulation(:,:)%life_disc_cost_nhome = 0
            allpopulation(:,:)%ydisable = -1
            allpopulation(:,:)%ydisease = -1
        	! initialize population
        	allocate(currentpop(nstartpop))
        	allocate(ntotpop(nyears))
        	currentpop = init_pop
        	npop = nstartpop
        	write(cr,"(i4)") r


			nextid = nstartpop
			!Go through each year
			do t = 1, nyears, 1
        		ntotpop(t)=npop
        		rnyears=t
        		year = startyear + gapyears*(t-1)
        		write(*,*) year
						if (updhcare) then
        			call updatehcare(year,npop,currentpop)
						endif
        		!Save data
				allocate(popallyear(t)%pop(npop))
				popallyear(t)%pop = currentpop
				do i = 1, npop ,1
				    allpopulation(t,currentpop(i)%id)= currentpop(i)
				end do


        		if (t .lt. nyears) then
					! perform transition
					call nextyear(year, npop, currentpop, nsurv_pop)
					if (nsurv_pop .eq. 0) then
						exit
					else
						! find size of new pop
						if (t .lt. ncycle .and. newcohort) then
								nnewpop = nsurv_pop + ncohortsize
						else
							nnewpop = nsurv_pop
						end if

						allocate(nextpop(nnewpop))
						! keep survivors
						ii = 1
						do i = 1, npop, 1
							if (currentpop(i)%alive .eq. 1) then
								nextpop(ii) = currentpop(i)
								ii = ii + 1
							end if
						end do

						! bring new cohort
						if (newcohort) then
							if (t .lt. ncycle) then
									thisnewpop = newpop
									call getnewcohort(thisnewpop, year,nextid,r)
								    nextid = nextid + ncohortsize

									do i = 1, ncohortsize, 1
										nextpop(ii) = thisnewpop(i)
										nextpop(ii)%byear = year - nextpop(ii)%age+gapyears
										provpos = nextpop(ii)%province
										sexpos = nextpop(ii)%sex+1
										! adjust for size of entering cohort (census)
										nextpop(ii)%wgt = nextpop(ii)%wgt*pop30trend(sexpos,t,provpos)
										ii = ii + 1
									end do
							end if
						end if

						! net migration
					    if (immigration) then
					    	call addimmig(year, nextpop, nnewpop)
						end if

						! do switch
						npop = nnewpop
						deallocate(currentpop)
						allocate(currentpop(npop))
						currentpop = nextpop
						deallocate(nextpop)

					end if
        		end if
        	end do

            !Count the total number of observations
            nobs = 0
            do j=1,rnyears,1
                nobs = size(popallyear(j)%pop)+nobs
            end do

            !Generate a unique vectors of persons to export with statamod

            allocate(allpersons(nobs))
            i = 1
            do j=1,rnyears,1
                allpersons(i:i+size(popallyear(j)%pop)-1) = popallyear(j)%pop
                i = i+size(popallyear(j)%pop) ! Update the index
            end do

            do t = 1, rnyears, 1
                deallocate(popallyear(t)%pop)
            end do
            start_time = omp_get_wtime()
            call computestats(allpopulation,rnyears,nyears,npoptot)
            stop_time = omp_get_wtime()
            print *, "Computestats time:", stop_time - start_time, "seconds"

						if (exportstata_opt) then
							call exportstata(allpersons,nobs,cr,rnyears)
						end if
            deallocate(ntotpop)
            deallocate(allpersons)
        end subroutine runonepopreps

        subroutine computestats(allpopulation,rnyears,nyears,n)
            integer rnyears,nyears, t,i,j,l,k, n,tid,minyear, nyears_tot
            type (person) :: thispop(n), thispop2(n)
            double precision, allocatable:: var(:),var2(:), byvar(:),byvar2(:),sex(:), wgt(:), evernhome(:)
            integer :: alive(n)
            logical, allocatable:: thismask(:),thismask_first(:),thismask_sec(:),thismask_third(:)
            double precision this, ival, totwgt
            type (person) allpopulation(nyears,n)
            type (person), allocatable :: simpop(:,:)
						character*80 :: sex_c, evernhome_c

            call longitudinalstats(allpopulation,rnyears,nyears,n)
            allocate(simpop(n,rnyears))
            allocate(var(n))
						allocate(var2(n))
            allocate(byvar(n))
            allocate(byvar2(n))
            allocate(sex(n))
            allocate(evernhome(n))
            allocate(wgt(n))
            allocate(thismask(n))
            allocate(thismask_first(n))
            allocate(thismask_sec(n))
            allocate(thismask_third(n))

            simpop =  transpose(allpopulation(1:rnyears,:))
            minyear = min(rnyears,nyearstats)
			sex_c = 'sex'
			evernhome_c = 'ever_nhome'
            do t = 1, minyear, 1

                thispop = simpop(:,t)
								if (t .lt. minyear) then
									thispop2 = simpop(:,t+1)
								end if
                alive = thispop(:)%alive
                thismask_first =  (alive .eq. 1)
                call extractvar(thispop,n,sex_c,sex)
                call extractvar(thispop,n,evernhome_c,evernhome)

                do i = 1, nstats,1

                    ! extract the var

										call extractvar(thispop,n,stats(i)%varname,var)

										if ((stats(i)%outname(1:2) .eq. 'i_') .or. (stats(i)%outname(1:2) .eq. 'r_')) then
											call extractvar(thispop2,n,stats(i)%varname,var2)
											if (stats(i)%outname(1:2) .eq. 'i_') then
												where (var2 .gt. var)
													var = 1
												elsewhere
													var = 0
												end where
											else
												where (var2 .lt. var)
													var = 1
												elsewhere
													var = 0
												end where
											end if
										end if

                    ! extract the by var
                    call extractvar(thispop,n,stats(i)%byvarname,byvar)

                    if (stats(i)%byvarname2 .ne. 'NULL') then
                        call extractvar(thispop,n,stats(i)%byvarname2,byvar2)
                    end if
                    ! set the weighting variable
                    if (stats(i)%wgt) then
                        wgt(:) = thispop(:)%wgt
                    else
                        wgt(:) = 1.0d0
                    end if

                    if (stats(i)%varname .eq. 'ydisable') then
                        thismask_sec = ((thismask_first .eqv. .TRUE.)  .AND. (var .ne. -1))
                    else if (stats(i)%varname .eq. 'ydisease') then
                        thismask_sec = ((thismask_first .eqv. .TRUE.)  .AND. (var .ne. -1))
                    else
                    thismask_sec = thismask_first
                    end if

!$OMP PARALLEL PRIVATE(thismask_first,thismask,thismask_third,ival,k,totwgt,this,tid) &
!$OMP  SHARED(n,t,nreps,thispop,stats,wgt,var,byvar,byvar2,alive,thismask_sec)
!$OMP DO
                    do j = 1, stats(i)%nbyvar, 1
                        if (stats(i)%byvarname .eq. 'year') then
                            thismask_first = thismask_sec
                        else
                            ival = dble(stats(i)%minbyvar + j - 1)
                            thismask_first = ((thismask_sec .eqv. .TRUE.) .AND. (byvar .eq. ival))
                        end if
                        do k = 1, stats(i)%nbyvar2, 1
                            if (stats(i)%byvarname2 .ne. 'NULL') then
                                ival = dble(stats(i)%minbyvar2 + k -1)
                                thismask = ((byvar2 .eq. ival) .AND. (thismask_first .eqv. .TRUE.))
                            else
                                thismask = thismask_first
                            end if
                            if (stats(i)%outname .eq. 'ydeadcvm') then
                            	thismask_third = ((sex .eq. 0.0d0) .AND. (thismask .eqv. .TRUE.))
                    					thismask = thismask_third
                    				end if

														if (stats(i)%outname .eq. 'ydeadcvw') then
                            	thismask_third = ((sex .eq. 1.0d0) .AND. (thismask .eqv. .TRUE.))
                    					thismask = thismask_third
														end if
					        					if (stats(i)%outname(1:2) .eq. 'h_') then
                                thismask_third = ((sex .eq. 0.0d0) .AND. (thismask .eqv. .TRUE.))
                                thismask = thismask_third
                            end if
                            if (stats(i)%outname(1:2) .eq. 'f_') then
                                thismask_third = ((sex .eq. 1.0d0) .AND. (thismask .eqv. .TRUE.))
                                thismask = thismask_third
                            end if
                            if (stats(i)%outname(1:2) .eq. 'e_') then
                                thismask_third = ((evernhome .eq. 1.0d0) .AND. (thismask .eqv. .TRUE.))
                                thismask = thismask_third
                            end if
                            totwgt = sum(wgt(:), mask = thismask)
                            if (totwgt .gt. 0.0d0) then
                                if (stats(i)%cmd .eq. 'sum') then
                                    this = sum(wgt(:) * var(:), mask = thismask)
                                else if (stats(i)%cmd .eq. 'mean') then
                                    this = sum(wgt(:) * var(:), mask = thismask)/totwgt
                                end if
                            else
                                this = 0.0d0
                            end if

                            stats(i)%value(t,j,k) = stats(i)%value(t,j,k) + this/dble(nreps)
                            stats(i)%sd(t,j,k) = stats(i)%sd(t,j,k) + (this**2)/dble(nreps)

                        end do
                    end do
!$OMP END DO
!$OMP END PARALLEL


               end do

            end do


        end subroutine computestats
        subroutine longitudinalstats(allpopulation,rnyears,nyears,n)
            integer rnyears,nyears, t,i,n
            type (person) allpopulation(nyears,n)
           !First run for last year becaus t+1 doesn't exist
           do i = 1, n , 1
                if (allpopulation(rnyears,i)%alive .eq. 1) then
                    allpopulation(rnyears,i)%nyears =  2

                    if (allpopulation(rnyears,i)%inv .eq. 1) then
                        allpopulation(rnyears,i)%nyears_disable = 2
                    else
                        allpopulation(rnyears,i)%nyears_disable = 0
                    end if
                    if (allpopulation(rnyears,i)%diabe .eq. 1 .OR. allpopulation(rnyears,i)%hibpe .eq. 1 .OR. &
                        allpopulation(rnyears,i)%hearte .eq. 1 .OR. allpopulation(rnyears,i)%stroke .eq. 1 .OR. &
                        allpopulation(rnyears,i)%cancre .eq. 1 .OR. allpopulation(rnyears,i)%lunge .eq. 1 .OR. &
                        allpopulation(rnyears,i)%mentae .eq. 1) then

                        allpopulation(rnyears,i)%nyears_disease = 0
                    else
                        allpopulation(rnyears,i)%nyears_disease = 2
                    end if
                    if ( allpopulation(rnyears,i)%ltc .eq. 3 ) then
                    	allpopulation(rnyears,i)%nyears_nhome = 2
                	else
                		allpopulation(rnyears,i)%nyears_nhome = 0
                	end if
                 end if
            end do
            ! Go through all
            do i = 1, n , 1
                do t = rnyears-1,1,-1
                    if (allpopulation(t,i)%alive .eq. 1) then
                        allpopulation(t,i)%nyears = allpopulation(t+1,i)%nyears + 2

                        if (allpopulation(t,i)%inv .eq. 1) then
                            allpopulation(t,i)%nyears_disable = allpopulation(t+1,i)%nyears_disable+2
                            if (allpopulation(t,i)%nyears_disable .eq. 2) then
                                allpopulation(t,i)%nyears_disable = 1
                            end if
                        else
                            allpopulation(t,i)%nyears_disable = allpopulation(t+1,i)%nyears_disable
                        end if

                        if (allpopulation(t,i)%diabe .eq. 1 .OR. allpopulation(t,i)%hibpe .eq. 1 .OR. &
                            allpopulation(t,i)%hearte .eq. 1 .OR. allpopulation(t,i)%stroke .eq. 1 .OR. &
                            allpopulation(t,i)%cancre .eq. 1 .OR. allpopulation(t,i)%lunge .eq. 1 .OR. &
                            allpopulation(t,i)%mentae .eq. 1) then
                            allpopulation(t,i)%nyears_disease = allpopulation(t+1,i)%nyears_disease
                        else
                            allpopulation(t,i)%nyears_disease = allpopulation(t+1,i)%nyears_disease+2
                            if (allpopulation(t,i)%nyears_disease .eq. 2) then
                                allpopulation(t,i)%nyears_disease  = 1
                            end if
                        end if
                        if ( allpopulation(t,i)%ltc .eq. 3 ) then
                    		allpopulation(t,i)%nyears_nhome = allpopulation(t+1,i)%nyears_nhome +2

                		else
                			allpopulation(t,i)%nyears_nhome = allpopulation(t+1,i)%nyears_nhome
                		end if
                		if (allpopulation(t,i)%nyears_nhome .eq. 2) then
                                allpopulation(t,i)%nyears_nhome  = 1
                        end if
                     end if
                 end do
             end do
             do i = 1, n , 1
                do t = rnyears,1,-1
                    if (allpopulation(t,i)%alive .eq. 1) then
                        if (allpopulation(t,i)%nyears_disable .eq. 0) then
                            allpopulation(t,i)%ydisable = -1
                        else if (allpopulation(t,i)%nyears_disable .ne. 1) then
                            allpopulation(t,i)%ydisable = 0
                        else
                            allpopulation(t,i)%ydisable = 1
                        end if

                        if (allpopulation(t,i)%nyears_disease .eq. 0) then
                            allpopulation(t,i)%ydisease = -1
                        else if (allpopulation(t,i)%nyears_disease .ne. 1) then
                            allpopulation(t,i)%ydisease = 0
                        else
                            allpopulation(t,i)%ydisease = 1
                        end if
                    end if
                 end do
             end do

        end subroutine longitudinalstats

        subroutine extractvar(simpop, nobs, varname, var)
            integer nobs,i
            type (person) simpop(nobs)
            double precision var(nobs)
            character*80 varname
            ! variables are from record
            if (varname .eq. 'id') then
                var = dble(simpop(:)%id)
            else if (varname .eq. 'one') then
                var = 1
            else if (varname .eq. 'age') then
                var = dble(simpop(:)%age)
            else if (varname .eq. 'age51') then
                do i=1, nobs,1
                    if (simpop(i)%age .ge. 51) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'age65') then
                do i=1, nobs,1
                    if (simpop(i)%age .ge. 65) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'age70') then
                do i=1, nobs,1
                    if (simpop(i)%age .ge. 70) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
           else if (varname .eq. 'age6574') then
                do i=1, nobs,1
                    if (simpop(i)%age .ge. 65 .AND. simpop(i)%age .le. 74) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
          else if (varname .eq. 'age3031') then
    									 do i=1, nobs,1
    											 if (simpop(i)%age .ge. 30 .AND. simpop(i)%age .le. 31) then
    													 var(i) = 1
    											 else
    													 var(i) = 0
    											 end if
    									 end do
					else if (varname .eq. 'age3034') then
									 do i=1, nobs,1
											 if (simpop(i)%age .ge. 30 .AND. simpop(i)%age .le. 34) then
													 var(i) = 1
											 else
													 var(i) = 0
											 end if
									 end do
					else if (varname .eq. 'age7075') then
 	                do i=1, nobs,1
 	                    if (simpop(i)%age .ge. 70 .AND. simpop(i)%age .le. 75) then
 	                        var(i) = 1
 	                    else
 	                        var(i) = 0
 	                    end if
 	                end do
           else if (varname .eq. 'age6070') then
                do i=1, nobs,1
                    if (simpop(i)%age .ge. 60 .AND. simpop(i)%age .le. 70) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'byear') then
                var = dble(simpop(:)%byear)
            else if (varname .eq. 'sex') then
                var = dble(simpop(:)%sex)
            else if (varname .eq. 'wgt') then
                var = simpop(:)%wgt
            else if (varname .eq. 'province') then
                var = simpop(:)%province
            else if (varname .eq. 'year') then
                var = simpop(:)%year
            else if (varname .eq. 'imm') then
                    var = simpop(:)%imm
           else if (varname .eq. 'educ') then
                    var = simpop(:)%educ
           else if (varname .eq. 'lesshs') then
                do i=1, nobs,1
                    var(i) = dummy(simpop(i)%educ,1)
                end do
            else if (varname .eq. 'hs') then
                do i=1, nobs,1
                    var(i) = dummy(simpop(i)%educ,2)
                end do
            else if (varname .eq. 'college') then
                do i=1, nobs,1
                    var(i) = dummy(simpop(i)%educ,3)
                end do
            else if (varname .eq. 'university') then
                do i=1, nobs,1
                    var(i) = dummy(simpop(i)%educ,4)
                end do
           else if (varname .eq. 'hsless') then
                do i=1, nobs,1
                    if (simpop(i)%educ .le. 2) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'diabe') then
                var = simpop(:)%diabe
            else if (varname .eq. 'hibpe') then
                var = simpop(:)%hibpe
            else if (varname .eq. 'hearte') then
                var = simpop(:)%hearte
            else if (varname .eq. 'stroke') then
                var = simpop(:)%stroke
            else if (varname .eq. 'cancre') then
                var = simpop(:)%cancre
            else if (varname .eq. 'lunge') then
                var = simpop(:)%lunge
            else if (varname .eq. 'mentae') then
                var = simpop(:)%mentae
            else if (varname .eq. 'idiabe') then
                var = simpop(:)%idiabe
            else if (varname .eq. 'ihibpe') then
                var = simpop(:)%ihibpe
            else if (varname .eq. 'ihearte') then
                var = simpop(:)%ihearte
            else if (varname .eq. 'istroke') then
                var = simpop(:)%istroke
            else if (varname .eq. 'icancre') then
                var = simpop(:)%icancre
            else if (varname .eq. 'ilunge') then
                var = simpop(:)%ilunge
            else if (varname .eq. 'imentae') then
                var = simpop(:)%imentae
            else if (varname .eq. 'cardiov') then
                do i=1, nobs,1
                    if (simpop(i)%hibpe .eq. 1 .OR. simpop(i)%hearte .eq. 1 .OR. simpop(i)%stroke .eq. 1) then
                    	var(i) = 1
                    else
                    	var(i) = 0
                    end if
                end do
            else if (varname .eq. 'cvd') then
                  do i=1, nobs,1
                      if (simpop(i)%hearte .eq. 1 .OR. simpop(i)%stroke .eq. 1) then
                      	var(i) = 1
                      else
                      	var(i) = 0
                      end if
                  end do
            else if (varname .eq. 'otherdisease') then
                do i=1, nobs,1
                    if ((simpop(i)%hibpe + simpop(i)%stroke + simpop(i)%lunge + simpop(i)%mentae) .ge. 1) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'disease1p') then
                do i=1, nobs,1
                    if (( simpop(i)%diabe + simpop(i)%hearte + simpop(i)%stroke + simpop(i)%cancre &
												+ simpop(i)%lunge + simpop(i)%mentae) .ge. 1) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
							else if (varname .eq. 'disease2p') then
	                do i=1, nobs,1
	                    if (( simpop(i)%diabe + simpop(i)%hearte + simpop(i)%stroke + simpop(i)%cancre &
													+ simpop(i)%lunge + simpop(i)%mentae) .ge. 2) then
	                        var(i) = 1
	                    else
	                        var(i) = 0
	                    end if
	                end do
						else if (varname .eq. 'disease0') then
	                do i=1, nobs,1
	                    if ((simpop(i)%diabe + simpop(i)%hearte + simpop(i)%stroke + simpop(i)%cancre &
	                        + simpop(i)%lunge + simpop(i)%mentae) .eq. 0) then
	                        var(i) = 1
	                    else
	                        var(i) = 0
	                    end if
	                end do
						else if (varname .eq. 'disease1') then
			                do i=1, nobs,1
			                    if (( simpop(i)%diabe + simpop(i)%hearte + simpop(i)%stroke + simpop(i)%cancre &
			                        + simpop(i)%lunge + simpop(i)%mentae) .eq. 1) then
			                        var(i) = 1
			                    else
			                        var(i) = 0
			                    end if
			                end do
						else if (varname .eq. 'disease2') then
		                do i=1, nobs,1
		                    if (( simpop(i)%diabe + simpop(i)%hearte + simpop(i)%stroke + simpop(i)%cancre &
		                        + simpop(i)%lunge + simpop(i)%mentae) .eq. 2) then
		                        var(i) = 1
		                    else
		                        var(i) = 0
		                    end if
		                end do
            else if (varname .eq. 'disease3') then
                do i=1, nobs,1
                    if (( simpop(i)%diabe + simpop(i)%hearte + simpop(i)%stroke + simpop(i)%cancre &
												+ simpop(i)%lunge + simpop(i)%mentae) .ge. 3) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'diseasenum') then
                do i=1, nobs,1
                    var(i)= simpop(i)%diabe + simpop(i)%hearte + simpop(i)%stroke + simpop(i)%cancre &
												+ simpop(i)%lunge + simpop(i)%mentae
                end do
            else if (varname .eq. 'nvrsmoke') then !Three categories
                do i=1, nobs,1
                    var(i) = dummy(simpop(i)%smoke,1)
                end do
           else if (varname .eq. 'smoker') then
                do i=1, nobs,1
                    var(i) = dummy(simpop(i)%smoke,2)
                end do
            else if (varname .eq. 'former') then
                do i=1, nobs,1
                    var(i) = dummy(simpop(i)%smoke,3)
                end do
            else if (varname .eq. 'nonobese') then !BMI
                do i=1, nobs,1
                    var(i) = dummy(simpop(i)%bmi,1)
                end do
            else if (varname .eq. 'obese') then !BMI
                do i=1, nobs,1
                    var(i) = dummy(simpop(i)%bmi,2)
                end do
            else if (varname .eq. 'veryobese') then !BMI
                do i=1, nobs,1
                    var(i) = dummy(simpop(i)%bmi,3)
                end do
            else if (varname .eq. 'inv') then
                var = simpop(:)%inv
            else if (varname .eq. 'iadl') then
                do i=1, nobs,1
                    if (simpop(i)%inv .eq.3 .OR. simpop(i)%inv .eq. 5 .OR. simpop(i)%inv .eq. 6 .OR. simpop(i)%inv .eq. 7) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'adl') then
                do i=1, nobs,1
                    if (simpop(i)%inv .eq.4 .OR. simpop(i)%inv .eq. 6 .OR. simpop(i)%inv .eq. 7) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'cognitive') then
                do i=1, nobs,1
                    if (simpop(i)%inv .eq.2 .OR. simpop(i)%inv .eq. 5 .OR. simpop(i)%inv .eq. 7) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'disable') then
                do i=1, nobs,1
                    if (simpop(i)%inv .ne. 1 .AND. simpop(i)%inv .ne. 2 ) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'alive') then
                var = simpop(:)%alive
            else if (varname .eq. 'ydead') then
                do i=1,nobs,1
                    if (simpop(i)%nyears .eq. 1) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
           else if (varname .eq. 'ltc_need') then
                do i=1,nobs,1
                    if (simpop(i)%ltc .ge. 2 ) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
           else if (varname .eq. 'ltc') then
                var = simpop(:)%ltc
           else if (varname .eq. 'homecare') then
               do i=1,nobs,1
                   if (simpop(i)%ltc .eq. 2) then
                       var(i) = 1
                   else
                       var(i) = 0
                   end if
               end do
            ! Longitudinal
            else if (varname .eq. 'nyears') then
                var = simpop(:)%nyears
            else if (varname .eq. 'nyears_disable') then
                var = simpop(:)%nyears_disable
            else if (varname .eq. 'nyears_disease') then
                var = simpop(:)%nyears_disease
            else if (varname .eq. 'nyears_nhome') then
                var = simpop(:)%nyears_nhome
            else if (varname .eq. 'ever_nhome') then
               do i=1,nobs,1
                    if (simpop(i)%nyears_nhome .ge. 1) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'ydisable') then
                var = simpop(:)%ydisable
            else if (varname .eq. 'ydisease') then
                var = simpop(:)%ydisease

            ! Use
            else if (varname .eq. 'hc_nights') then
                var = simpop(:)%hc_nights
            else if (varname .eq. 'nights1') then
                do i=1,nobs,1
                if (simpop(i)%hc_nights .ge. 1) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'hc_specialist') then
                var = simpop(:)%hc_specialist
            else if (varname .eq. 'hc_generalist') then
                var = simpop(:)%hc_generalist
            else if (varname .eq. 'doctorvisit') then
                do i=1,nobs,1
                    var(i) = simpop(i)%hc_generalist + simpop(i)%hc_specialist
                end do
            else if (varname .eq. 'hc_hui') then
                var = simpop(:)%hc_hui
            else if (varname .eq. 'hc_drugs') then
                var = simpop(:)%hc_drugs
            else if (varname .eq. 'hc_homecare_f') then
                var = simpop(:)%hc_homecare_f
            else if (varname .eq. 'hc_homecare_i') then
                var = simpop(:)%hc_homecare_i
            else if (varname .eq. 'hc_homecare_sp') then
                var = simpop(:)%hc_homecare_sp
            ! Cost
            else if (varname .eq. 'cost_nights') then
                var = simpop(:)%cost_nights
            else if (varname .eq. 'cost_specialist') then
                var = simpop(:)%cost_specialist
            else if (varname .eq. 'cost_generalist') then
                var = simpop(:)%cost_generalist
            else if (varname .eq. 'cost_drugs') then
                var = simpop(:)%cost_drugs
            else if (varname .eq. 'cost_homecare_f') then
                var = simpop(:)%cost_homecare_f
            else if (varname .eq. 'hours_homecare_f') then
                var = simpop(:)%hours_homecare_f
            else if (varname .eq. 'hours_homecare_i') then
                var = simpop(:)%hours_homecare_i
            else if (varname .eq. 'hours_homecare_sp') then
                var = simpop(:)%hours_homecare_sp
            else if (varname .eq. 'homecare_f') then
                do i=1,nobs,1
                    if (simpop(i)%hc_homecare_f .eq. 1 .AND. simpop(i)%hc_homecare_i .eq. 0) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'homecare_i') then
                do i=1,nobs,1
                    if (simpop(i)%hc_homecare_f .eq. 0 .AND. simpop(i)%hc_homecare_i .eq. 1) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'homecare_f_i') then
                do i=1,nobs,1
                    if (simpop(i)%hc_homecare_f .eq. 1 .AND. simpop(i)%hc_homecare_i .eq. 1) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do
            else if (varname .eq. 'cost_nhome') then
                var = simpop(:)%cost_nhome
            else if (varname .eq. 'income') then
                var = simpop(:)%income
            else if (varname .eq. 'homecare') then
                do i=1, nobs,1
                    var(i) = dummy(simpop(i)%ltc,2)
                end do
            else if (varname .eq. 'homecare_w_fi') then
                do i=1, nobs,1

                   if (simpop(i)%ltc .eq. 2 .AND. simpop(i)%hc_homecare_f .eq. 0 .AND. simpop(i)%hc_homecare_i .eq. 0 ) then
                        var(i) = 1
                    else
                        var(i) = 0
                    end if
                end do

            else if (varname .eq. 'nhome') then
                do i=1, nobs,1
                    var(i) = dummy(simpop(i)%ltc,3)
                end do
            else
                do i=1, nobs,1
                    var(i) = 0
                end do
            end if
        end subroutine extractvar


        subroutine exportstats(coef)
            integer i, t, j,jj,k,kk, year,coef
            character*4 cat
            character*4 fmt
            character*100 outvalue
            character*100 outsd
            character(len=4) :: cr

						write(cr,"(i4)") coef
						write(*,*) "parameter set : ", cr

            open(3,file=adjustl(trim(outpath))//'/'//'create_'// &
                adjustl(trim(scenario))//'.do')
            write(3,*) '*do-file for creating results dataset'
            write(3,*) 'capture log close'
            write(3,*) 'version 13.1'
            write(3,*) 'clear all'
            write(3,*) 'set more off'

            write(3,*) ' '
            do i = 1, nstats, 1
                outvalue = adjustl(trim(outpath))//'/'// &
                    adjustl(trim(stats(i)%outname)) &
                    //'_'//adjustl(trim(stats(i)%cmd)) &
                    //'_'//adjustl(trim(stats(i)%byvarname))//'_value'
                open(1,file=adjustl(trim(outvalue))//'.dat', action = 'write')
                open(2,file=adjustl(trim(outvalue))//'.dct', action = 'write')
                fmt = '(i4)'
                write(2,*) 'dictionary using '//adjustl(trim(outvalue))//'.dat {'
                write(2,*) 'int ','year'
                write(2,*) 'double ',adjustl(trim(stats(i)%outname))
                write(2,*) 'double ',adjustl(trim(stats(i)%outname))//'_sd'
                if (stats(i)%byvarname .ne. 'year') then
                    write(2,*) 'int ',adjustl(trim(stats(i)%byvarname))
                end if
                if (stats(i)%byvarname2 .ne. 'NULL') then

                    write(2,*) 'int ',adjustl(trim(stats(i)%byvarname2))
                end if

                write(2,*) '}'
                close(2)

                do j = 1,stats(i)%nbyvar2,stats(i)%gap2
                    do k = 1,stats(i)%nbyvar,stats(i)%gap
                        do t = 1, nyears, 1
                            kk = stats(i)%minbyvar+k*stats(i)%gap-stats(i)%gap
                            jj = stats(i)%minbyvar2+j*stats(i)%gap2-stats(i)%gap2

                            year = startyear + t*gapyears  - gapyears
                            stats(i)%sd(t,k,j) = dsqrt(stats(i)%sd(t,k,j) - stats(i)%value(t,k,j)**2)
                            write(1,*) year, stats(i)%value(t,k,j), stats(i)%sd(t,k,j),kk,jj
                        end do
                    end do
                end do
                close(1)
                write(3,*) 'infile using ',adjustl(trim(outvalue)),'.dct, clear'
                write(3,*) 'if c(stata_version)>=14.0  saveold ' &
                ,adjustl(trim(outvalue))//trim(adjustl(cr)),'.dta, replace version(13)'
                write(3,*) 'if c(stata_version)<14.0 save ', &
                adjustl(trim(outvalue))//trim(adjustl(cr)),'.dta, replace'
                !write(3,*) 'infile using ',adjustl(trim(outsd)),'.dct, clear'
                !write(3,*) 'save ',adjustl(trim(outsd)),'.dta, replace'
                !write(3,*) 'use ',adjustl(trim(outpath)),'/results_',adjustl(trim(scenario)),'.dta'
                !write(3,*) 'merge 1:1 year using ',adjustl(trim(outvalue)),'.dta, nogen'
                !write(3,*) 'merge 1:1 year using ',adjustl(trim(outsd)),'.dta, nogen'
                !write(3,*) 'save ',adjustl(trim(outpath)),'/results_',adjustl(trim(scenario)),'.dta, replace'
            end do
            close(3)
            call system(STATA//' -b '//adjustl(trim(outpath))//'/'//'create_'// &
                adjustl(trim(scenario))//'.do')
            call system('rm '//adjustl(trim(outpath))//'/*.dct '//adjustl(trim(outpath))//'/*.dat')
        end subroutine exportstats

        subroutine changeflag
            integer endfile
            double precision diff
            double precision value1,value2
            integer year,flag
            character*20 variable,lastcommit
            call system('rm '//adjustl(trim(path))//'compare/current/*')
            call system('cp '//adjustl(trim(outpath))//'/* '//adjustl(trim(path))//'compare/current/')
            open(1,file=adjustl(trim(path))//'compare/list_commits.dat')
            read(1,*,IOSTAT=endfile) lastcommit
            close(1)
            if(endfile .lt. 0) then
                write(*,*) "No previous run in list_commits.dat - No comparison possible"
                return
            end if
            write(*,*) lastcommit
						write(*,*)'Stata :',STATA
            call system(STATA//' -b '//adjustl(trim(path))//'do/stats/profile_compas_table.do current '// &
                adjustl(trim(lastcommit))//' ' //adjustl(trim(path))//'/compare/')
            open(1,file=adjustl(trim(path))//'compare/rapport/list.dat')
            write(*,"(A1,A9,11x,A4,4x,A9,1x,A9,2x,A6,4x,A4)")" ", "Variables","Year","Benchmark",trim(scenario) // "      ",&
                "% var.","flag"
            do while(1 .eq. 1)

                read(1,*,IOSTAT=endfile) variable,year,value1,value2,diff,flag
                if(endfile .lt. 0) then
                    EXIT
                else
                    write(*,"(A1,A20,I4,4x,F6.3,4x,F6.3,3x,F6.2,3x,I1)") " ",variable,year, value1,value2,diff,flag
                end if
            end do
        end subroutine changeflag

      subroutine exportstata(allpersons,nobs,cr,rnyears)
      type (person), allocatable :: allpersons(:), byearpersons(:)
 			character(len=4) :: cr
 			integer, allocatable :: years(:)
 			integer :: i,j,rnyears
			integer :: n,nobs
			type (person) :: temppersons(nobs)
			integer*1, allocatable :: tmp_int1(:)

			!Count number of person for birth year
			n = 0
			j = 1
			do i = 1,nobs
					if (allpersons(i)%byear .ge. start_byear .and. allpersons(i)%byear .le. end_byear) then
						n = n +1
						temppersons(j)=allpersons(i)
						j = j+1
					end if
			end do


			allocate(byearpersons(n))
    	byearpersons = temppersons(1:n)

			!Save variables in the wright format
			allocate(tmp_int1(n)) !Use to save in stata Byte format

			call saveStata(adjustl(trim(path))//'output/'//adjustl(trim(scenario))//'/simpop-'//trim(adjustl(cr))//'.dta',1,n)

			call writestata(byearpersons%year,'year')
			call writestata(byearpersons%id,'id')
			call writestata(byearpersons%age,'age')
			!call writestata(byearpersons%byear,'byear')
			tmp_int1= byearpersons%educ
			call writestata(tmp_int1,'educ')
			tmp_int1= byearpersons%sex
			call writestata(tmp_int1,'sex')
			!tmp_int1= byearpersons%imm
			!call writestata(tmp_int1,'imm')
			tmp_int1= byearpersons%province
			call writestata(tmp_int1,'province')
			tmp_int1= byearpersons%diabe
			 call writestata(tmp_int1,'diabe')
			tmp_int1= byearpersons%hibpe
			call writestata(tmp_int1,'hibpe')
			 tmp_int1= byearpersons%hearte
			call writestata(tmp_int1,'hearte')
			tmp_int1= byearpersons%stroke
			call writestata(tmp_int1,'stroke')
			tmp_int1= byearpersons%cancre
			call writestata(tmp_int1,'cancre')
			tmp_int1= byearpersons%lunge
			call writestata(tmp_int1,'lunge')
			tmp_int1= byearpersons%mentae
			call writestata(tmp_int1,'mentae')
			tmp_int1= byearpersons%smoke
			call writestata(tmp_int1,'smoke')
			tmp_int1= byearpersons%bmi
			call writestata(tmp_int1,'bmi')
			tmp_int1= byearpersons%inv
			call writestata(tmp_int1,'inv')
			tmp_int1= byearpersons%alive
			call writestata(tmp_int1,'alive')
			!call writestata(byearpersons%hc_nights,'hc_nights')
			!call writestata(byearpersons%hc_specialist,'hc_specialist')
			!call writestata(byearpersons%hc_generalist,'hc_generalist')
			!call writestata(byearpersons%hc_hui,'hc_hui')
			!tmp_int1= byearpersons%hc_drugs
			!call writestata(tmp_int1,'hc_drugs')
			!tmp_int1= byearpersons%hc_homecare_f
			!call writestata(tmp_int1,'hc_homecare_f')
			!tmp_int1= byearpersons%hc_homecare_i
			!call writestata(tmp_int1,'hc_homecare_i')
			!tmp_int1= byearpersons%hc_homecare_sp
			! call writestata(tmp_int1,'hc_homecare_sp')
			!call writestata(nint(byearpersons%cost_nights),'cost_nights')
			!call writestata(nint(byearpersons%cost_specialist),'cost_specialist')
			!call writestata(nint(byearpersons%cost_generalist),'cost_generalist')
			!call writestata(nint(byearpersons%cost_homecare_f),'cost_homecare_f')
			! call writestata(nint(byearpersons%hours_homecare_i),'hours_homecare_i')
			! call writestata(nint(byearpersons%hours_homecare_sp),'hours_homecare_sp')
			! call writestata(nint(byearpersons%cost_drugs),'cost_drugs')
			!call writestata(nint(byearpersons%cost_nhome),'cost_nhome')
			! tmp_int1= byearpersons%income
			! call writestata(tmp_int1,'income')
		  !tmp_int1= byearpersons%ltc
			!call writestata(tmp_int1,'ltc')
			call writestata(nint(byearpersons%wgt),'wgt')
			call closeSaveStata()

        end subroutine exportstata

        subroutine getnewcohort(pop, year,nextid,rep)
        	integer year, sexpos,i, j, k, s, yearpos,nextid,rep,size_seed
        	type (person) pop(ncohortsize)
        	double precision draw, eta(ncohortmodels), cumprob
        	double precision eps(ncohortmodels)
        	double precision xb(ncohortmodels), prob(ncohortmodels,10)
        	type (derivedvars) vars(nderivedvars)

        	yearpos = (year - startyear)/gapyears + 1


					call random_seed(size=size_seed)
					allocate(seed(size_seed))
					call random_seed(get=seed)
					seed =seed_cmp((rep-1)*ncycle+yearpos,:)
					call random_seed(put=seed)
					deallocate(seed)
        	! get standard normal draws (ncohortsize by newcohort_models)
        	do 	i = 1, ncohortsize, 1

        		pop(i)%year = year+gapyears
						sexpos = 1 + pop(i)%sex
        		do j = 1, ncohortmodels, 1
        			call random_number(draw)
        			eta(j) = qnorm(draw)
        		end do
        		! transform into multivariate normal and then back to uniform
        		do j = 1, ncohortmodels, 1
        			eps(j) = 0.0d0
        			do k = j, ncohortmodels, 1
        				eps(j) = eps(j) + newcohort_choleski(j,k)*eta(k)

        			end do
        			eps(j) = pnorm(eps(j))
        		end do
        		! get indices
        		xb(:) = 0.0d0
        		call getderivedvars(pop(i), vars,year)
        		do j = 1, ncohortmodels, 1
        			do k = 1, newcohort_models(j)%npar, 1
        				do s = 1, nderivedvars, 1
        					if (newcohort_models(j)%param%var(k) .eq. vars(s)%label) then
        						xb(j) = xb(j) + newcohort_models(j)%param%val(k)* &
        							vars(s)%value
        						exit
        					end if
        				end do
        			end do
        		end do

        		! probabilities
        		do j = 1, ncohortmodels
        			if (newcohort_models(j)%spec .eq. oprobit) then
        				do k = 1, newcohort_models(j)%nvalues, 1
        					prob(j,k) = pnorm(newcohort_models(j)%thres%val(k+1) - xb(j)) - &
        						pnorm(newcohort_models(j)%thres%val(k) - xb(j))
        				end do
        				! apply trend and draw
        				if (newcohort_models(j)%label .eq. 'educ') then
        					do k = 1, newcohort_models(j)%nvalues, 1
										prob(j,k) = prob(j,k)*trendnew(yearpos)%educ(k)*calibeduc(k,pop(i)%province,sexpos)

        					end do
        					if (sum(prob(j,1:newcohort_models(j)%nvalues)) .ne. 1.0d0) then


        						prob(j,1:newcohort_models(j)%nvalues) = prob(j,1:newcohort_models(j)%nvalues) &
        							/sum(prob(j,1:newcohort_models(j)%nvalues))
										end if

        					! draw
        					cumprob = 0.0d0
        					do k = 1, newcohort_models(j)%nvalues, 1
        						cumprob = cumprob + prob(j,k)
        						if (eps(j) .le. cumprob) then
        							pop(i)%educ = k
        							exit
        						end if
        					end do

        				else if (newcohort_models(j)%label .eq. 'smoke') then
        					do k = 1, newcohort_models(j)%nvalues, 1
        						prob(j,k) = prob(j,k)*trendnew(yearpos)%smoke(k)
        					end do
        					if (sum(prob(j,1:newcohort_models(j)%nvalues)) .ne. 1.0d0) then
        						!write(*,*) 'probabilities do not sum to 1'
        						prob(j,1:newcohort_models(j)%nvalues) = prob(j,1:newcohort_models(j)%nvalues) &
        							/sum(prob(j,1:newcohort_models(j)%nvalues))
        					end if
        					! draw
        					cumprob = 0.0d0
        					do k = 1, newcohort_models(j)%nvalues, 1
        						cumprob = cumprob + prob(j,k)
        						if (eps(j) .le. cumprob) then
        							pop(i)%smoke = k
        							exit
        						end if
        					end do

        				else if (newcohort_models(j)%label .eq. 'bmi') then
        					do k = 1, newcohort_models(j)%nvalues, 1
        						prob(j,k) = prob(j,k)*trendnew(yearpos)%bmi(k)
        					end do
        					if (sum(prob(j,1:newcohort_models(j)%nvalues)) .ne. 1.0d0) then
        						prob(j,1:newcohort_models(j)%nvalues) = prob(j,1:newcohort_models(j)%nvalues) &
        							/sum(prob(j,1:newcohort_models(j)%nvalues))
        					end if
        					! draw
        					cumprob = 0.0d0
        					do k = 1, newcohort_models(j)%nvalues, 1
        						cumprob = cumprob + prob(j,k)
        						if (eps(j) .le. cumprob) then
        							pop(i)%bmi = k
        							exit
        						end if
        					end do
        				end if
        			end if
        		end do
        		pop(i)%id = nextid + i

        	end do
					if (educ_intervention) then
						if (year .ge. 2034) then
							do 	i = 1, ncohortsize, 1
									call random_number(draw)
									if (pop(i)%educ .eq. 1) then
										if (draw .le. real(0.5)) then
												pop(i)%educ = 2
										end if
									else if (pop(i)%educ .eq. 2) then
										if (draw .le. real(0.3)) then
												pop(i)%educ = 3
										end if
									else if (pop(i)%educ .eq. 3) then
										if (draw .le. real(0.1)) then
												pop(i)%educ = 4
										end if
									end if
								end do
							end if
					end if
        end subroutine getnewcohort

        ! add immigrants by adjusting weights
        subroutine addimmig(year, pop, npop)
        	integer year, npop, yearpos, p, s, a, i
        	type (person) pop(npop)
        	integer pop_immig(5,2,nage_imm)
        	integer cut_age(4)
        	double precision pp
        	cut_age(1) = 30
        	cut_age(2) = 45
        	cut_age(3) = 65
        	cut_age(4) = 350
        	yearpos = (year - startyear)/gapyears + 1
        	do p = 1, 5, 1
        		do s = 1, 2, 1
        			do a = 1, nage_imm
        				pop_immig(p,s,a) = 0
        				do i = 1, npop, 1
        					if (pop(i)%province .eq. p .and. pop(i)%sex .eq. (s-1)  .and. &
        						pop(i)%age .ge. cut_age(a) .and. pop(i)%age .lt. cut_age(a+1) ) then
        						pop_immig(p,s,a) = pop_immig(p,s,a) + 1
        					end if
        				end do
        				if (pop_immig(p,s,a) .eq. 0 .or. immtrend(p,s,yearpos,a) .eq. 0.0d0) then
        					pp = 0.0d0
        				else
        					pp = immtrend(p,s,yearpos,a)/dble(pop_immig(p,s,a))
        				end if
        				if (pp .ne. 0.0d0) then
        					do i = 1, npop, 1
        						if (pop(i)%province .eq. p .and. pop(i)%sex .eq. (s-1) .and. &
        						pop(i)%age .ge. cut_age(a) .and. pop(i)%age .lt. cut_age(a+1) ) then
        							pop(i)%wgt = pop(i)%wgt + pp
        						end if
        					end do
        				end if

     				end do
        		end do
        	end do
        end subroutine addimmig

        ! compute transition probabilities
	subroutine getprobtrans(transit_models,ntransit,start_transit,stop_transit,vars, trans_prob,sexresp)
 			integer i,ii, j,jj, k,l, s, sexresp,sexpos, ntransit,start_param, stop_param,start_transit,stop_transit
 			type (derivedvars) vars(nderivedvars)
			type (prob) trans_prob(ntransit)
			type (model) transit_models(ntransit)
			integer ignpoi
			real gengam,gam,sgamma
			double precision sumprob

			do i = start_transit, stop_transit, 1
				ii =1+i - start_transit
				if (transit_models(i)%nsex .eq. 1) then
                    sexpos = 1
                    start_param  = 1
                    stop_param = transit_models(i)%neq
                else
                    if (sexresp .eq. 1) then
                        sexpos = 2
                        start_param  = transit_models(i)%neq+1
                        stop_param = 2*transit_models(i)%neq
                    else
                        sexpos = 1
                        start_param  = 1
                    stop_param = transit_models(i)%neq
                    end if

                end if
                ! compute indices


				do j = start_param, stop_param, 1
				    jj = 1 + j - start_param

					trans_prob(ii)%value(jj) = 0.0d0
					do k = 1, transit_models(i)%param(j)%npar, 1
						do s = 1, nderivedvars, 1
							if (transit_models(i)%param(j)%var(k) .eq. &
									vars(s)%label) then
								trans_prob(ii)%value(jj) = trans_prob(ii)%value(jj) + vars(s)%value * &
									transit_models(i)%param(j)%val(k)
								exit
							end if
						end do
					end do
				end do

				! compute probabilities
				if (transit_models(i)%spec .eq. cloglog) then
					trans_prob(ii)%value(1) = 1.0d0 - dexp(-dexp(trans_prob(ii)%value(1)))

				else if (transit_models(i)%spec .eq. logit) then
					trans_prob(ii)%value(1) = dexp(trans_prob(ii)%value(1)) / (1.0d0 + dexp(trans_prob(ii)%value(1)))

				else if (transit_models(i)%spec .eq. mlogit) then
					sumprob = sum(dexp(trans_prob(ii)%value(:)))
					do j = 1, transit_models(i)%neq, 1
						trans_prob(ii)%value(j) = dexp(trans_prob(ii)%value(j)) / sumprob
					end do
				else if(transit_models(i)%spec .eq. linear) then
                        trans_prob(ii)%value(1) = trans_prob(ii)%value(1)

        else if (transit_models(i)%spec .eq. poisson) then
                        trans_prob(ii)%value(1) = dexp(trans_prob(ii)%value(1))

				else if(transit_models(i)%spec .eq. negbin) then
												gam = sgamma(real(1/transit_models(i)%dispersion))
												if (gam .eq. 0) then
														trans_prob(ii)%value(1) = 0.0d0
												else
								                 trans_prob(ii)%value(1) = ignpoi(real(gam*transit_models(i)%dispersion*dexp(trans_prob(ii)%value(1))))
												end if
                end if
			end do


		end subroutine getprobtrans

		! calibrate at baseline census and implement trend
        subroutine transformprob(year, pop, trans_prob)
        	integer year, agepos, sexpos, yearpos, i
        	type (person) pop
        	type (prob) trans_prob(ntransit)
        	double precision rx, sx,px

        	agepos = pop%age - startage + 1
					yearpos = (year - startyear)/gapyears + 1
        	if (agepos .gt. nages) then
        			agepos = nages
        	end if
        	if (yearpos .gt. nyears) then
        			yearpos = nyears
        	end if
					sexpos = 1 + pop%sex
        	rx = mortalitytrend(pop%province, sexpos, yearpos, agepos)

        	! changes to mortality
        	do i = 1, ntransit, 1
				if(transit_models(i)%name .eq. 'alive') then
					! calibration to census and apply trend
					trans_prob(i)%value(1) = rx*calibmortality(agepos, sexpos)*trans_prob(i)%value(1)
					if (trans_prob(i)%value(1) .lt. 0.0d0) then
							trans_prob(i)%value(1) = 0.0d0
					else if (trans_prob(i)%value(1) .gt. 1.0d0) then
							trans_prob(i)%value(1) = 1.0d0
					end if

					exit
				end if
        	end do
        end subroutine transformprob

        ! function to compute indicator function
        integer function mapbin(draw, prob, state, elig, newmap)
        	double precision draw, prob
        	integer state, elig, newmap

        	if (state .eq. elig) then
        		if (draw .lt. prob) then
        			mapbin = newmap
        		else
        			mapbin = state
        		end if
        	else
        		mapbin = state
        	end if
        end function mapbin

        ! function to draw a multinomial outcome
        integer function mapmult(draw, prob, nstates, state)
        	integer state, newmap, nstates, j
        	double precision draw, prob(nstates), pp
        	pp = 0.0d0
        	mapmult = state
        	do j = 1, nstates, 1
        		pp = pp + prob(j)
        		if (draw .le. pp) then
        			mapmult = j
        			exit
        		end if
        	end do
        end function mapmult

        ! function that computes new states from probabilities
        subroutine newstates(year, currentpop, nextpop, trans_prob)
        	integer year, i, j
        	double precision draw, cureprob
        	logical cure
        	type (person) currentpop, nextpop
        	type (prob) trans_prob(ntransit)

        	nextpop%age = nextpop%age + gapyears
          	nextpop%year = year+gapyears
        	do i = 1, ntransit
        		cure = .false.
                ! prevention intervention
                if (ninterventions .gt. 0) then

                		do j = 1, ninterventions, 1
                			if (transit_models(i)%name .eq. interventions(j)%disease) then
                				if (interventions(j)%prevention) then
                					trans_prob(i)%value(1) = trans_prob(i)%value(1)*interventions(j)%prevention_prob
                				end if
                				if (interventions(j)%cure) then
                					cureprob = interventions(j)%cure_prob
                					call random_number(draw)
                					if (draw .lt. cureprob) then
                							cure = .true.
                					end if
                				end if

                			end if
                		end do
                end if
        		call random_number(draw)

        		! all binary outcomes
        		if (transit_models(i)%name .eq. 'alive') then
              !For scenario mortality
              if (decrcvd) then
                if ((currentpop%hearte .eq. 1 .OR. currentpop%stroke .eq. 1)) then
                  if (year .ge. 2014 .and. year .le. 2024 ) then
                      trans_prob(i)%value(1) = trans_prob(i)%value(1)*(1-(1-alpha)*.16/(6-(year-2014)/2))
                  else if (year .gt. 2024) then
                      trans_prob(i)%value(1) = trans_prob(i)%value(1)*(1-(1-alpha)*.16)
                  end if
              end if
             end if

        			nextpop%alive = mapbin(draw,trans_prob(i)%value(1), currentpop%alive, 1, 0)
        			if (cure) then
        				nextpop%alive = 1
        			end if
        			cycle
        		end if
        		if (transit_models(i)%name .eq. 'diabe') then
        			nextpop%diabe = mapbin(draw,trans_prob(i)%value(1),currentpop%diabe, 0, 1)
	            if (cure .and. nextpop%diabe .eq. 1) then
        				nextpop%diabe = 0
        			end if
              if (nextpop%diabe .eq. 1 .and. currentpop%diabe  .eq. 0 ) then
                nextpop%idiabe = 1
              else
                nextpop%idiabe = 0
              end if
        			cycle
        		end if
        		if (transit_models(i)%name .eq. 'hearte') then
              if (decrcvd) then
                  trans_prob(i)%value(1) = trans_prob(i)%value(1)* &
                  (1-alpha*min(.28/max((5-(year-2014)/2),1),.28))
              end if
                nextpop%hearte = mapbin(draw,trans_prob(i)%value(1),currentpop%hearte, 0, 1)
        			if (cure .and. nextpop%hearte .eq. 1) then
        				nextpop%hearte = 0
        			end if
              if (nextpop%hearte .eq. 1 .and. currentpop%hearte .eq. 0 ) then
                nextpop%ihearte = 1
              else
                nextpop%ihearte = 0
              end if

        			cycle
        		end if
						if (transit_models(i)%name .eq. 'rhearte') then
							if (currentpop%hearte .eq. 1) then
								nextpop%hearte = mapbin(draw,trans_prob(i)%value(1),currentpop%hearte, 1, 0)
							end if
							cycle
						end if
        		if (transit_models(i)%name .eq. 'hibpe') then
              if (decrcvd) then
                trans_prob(i)%value(1) = trans_prob(i)%value(1)* &
                (1-alpha*min(.28/max((5-(year-2014)/2),1),.28))
              end if
              nextpop%hibpe = mapbin(draw,trans_prob(i)%value(1),currentpop%hibpe, 0, 1)
       			if (cure .and. nextpop%hibpe .eq. 1) then
        				nextpop%hibpe = 0
        			end if
              if (nextpop%hibpe .eq. 1 .and. currentpop%hibpe .eq. 0 ) then
                nextpop%ihibpe = 1
              else
                nextpop%ihibpe = 0
              end if
        			cycle
        		end if
        		if (transit_models(i)%name .eq. 'cancre') then
        			nextpop%cancre = mapbin(draw,trans_prob(i)%value(1),currentpop%cancre, 0, 1)
        			if (cure .and. nextpop%cancre .eq. 1) then
        				nextpop%cancre = 0
        			end if
              if (nextpop%cancre .eq. 1 .and. currentpop%cancre  .eq. 0 ) then
                nextpop%icancre = 1
              else
                nextpop%icancre = 0
              end if
        			cycle
        		end if
						if (transit_models(i)%name .eq. 'rcancre') then
							if (currentpop%cancre .eq. 1) then
								nextpop%cancre = mapbin(draw,trans_prob(i)%value(1),currentpop%cancre, 1, 0)
							end if
							cycle
						end if
        		if (transit_models(i)%name .eq. 'stroke') then
							if (currentpop%age .lt. 40) then
								nextpop%stroke = 0
							else
                if (decrcvd) then
                  trans_prob(i)%value(1) = trans_prob(i)%value(1)* &
                  (1-alpha*min(.28/max((5-(year-2014)/2),1),.28))
                end if
								nextpop%stroke= mapbin(draw,trans_prob(i)%value(1),currentpop%stroke, 0, 1)
							end if
							if (cure .and. nextpop%stroke .eq. 1) then
        				nextpop%stroke = 0
        			end if
              if (nextpop%stroke .eq. 1 .and. currentpop%stroke  .eq. 0 ) then
                nextpop%istroke = 1
              else
                nextpop%istroke = 0
              end if
        			cycle
        		end if
						if (transit_models(i)%name .eq. 'rstroke') then
							if (currentpop%stroke .eq. 1) then
								nextpop%stroke= mapbin(draw,trans_prob(i)%value(1),currentpop%stroke, 1, 0)
							end if
							cycle
						end if
        		if (transit_models(i)%name .eq. 'lunge') then
        			nextpop%lunge = mapbin(draw,trans_prob(i)%value(1),currentpop%lunge, 0, 1)
        			if (cure .and. nextpop%lunge .eq. 1) then
        				nextpop%lunge = 0
        			end if
              if (nextpop%lunge .eq. 1 .and. currentpop%lunge  .eq. 0 ) then
                nextpop%ilunge = 1
              else
                nextpop%ilunge = 0
              end if
        			cycle
        		end if
        		if (transit_models(i)%name .eq. 'mentae') then
							if (currentpop%age .lt. 55) then
								nextpop%mentae = 0
							else
								nextpop%mentae = mapbin(draw,trans_prob(i)%value(1),currentpop%mentae, 0, 1)
							end if
        			if (cure .and. nextpop%mentae .eq. 1) then
        				nextpop%mentae = 0
        			end if
              if (nextpop%mentae .eq. 1 .and. currentpop%mentae  .eq. 0 ) then
                nextpop%imentae = 1
              else
                nextpop%imentae = 0
              end if
        			cycle
        		end if
        		if (transit_models(i)%name .eq. 'smoke_start' .and. currentpop%smoke .eq. 1) then
        			nextpop%smoke = mapbin(draw,trans_prob(i)%value(1),currentpop%smoke, 1, 2)
        			cycle
        		else if (transit_models(i)%name .eq. 'smoke_stop' .and. currentpop%smoke .eq. 2) then
        			nextpop%smoke = mapbin(draw,trans_prob(i)%value(1),currentpop%smoke, 2, 3)
        			if (cure .and. nextpop%smoke .eq. 2) then
        				nextpop%smoke = 3
        			end if
        			cycle
        		else if (transit_models(i)%name .eq. 'smoke_restart' .and. currentpop%smoke .eq. 3) then
        			nextpop%smoke = mapbin(draw,trans_prob(i)%value(1),currentpop%smoke, 3, 2)
        			cycle
        		end if
        		if (transit_models(i)%name .eq. 'bmi') then
        			nextpop%bmi = mapmult(draw,trans_prob(i)%value(:),transit_models(i)%neq,currentpop%bmi)
        			if (cure .and. nextpop%bmi .eq. 2) then
        				nextpop%bmi = 1
        			end if
        			if (cure .and. nextpop%bmi .eq. 3) then
        				nextpop%bmi = 2
        			end if
        			cycle
        		end if
        		if (transit_models(i)%name .eq. 'inv') then
        			nextpop%inv = mapmult(draw,trans_prob(i)%value(:),transit_models(i)%neq,currentpop%inv)
        			cycle
        		end if

        		if (transit_models(i)%name .eq. 'income') then
        			nextpop%income = mapmult(draw,trans_prob(i)%value(:),transit_models(i)%neq,currentpop%income)
        			cycle
        		end if

        		if (transit_models(i)%name .eq. 'ltc') then
							if (currentpop%ltc .eq. 3) then
								nextpop%ltc = 3
							else
        				nextpop%ltc = mapmult(draw,trans_prob(i)%value(:),transit_models(i)%neq,currentpop%ltc)
							end if
        			cycle
        		end if
        	end do
        end subroutine newstates

        ! function that makes a transition, uses OPENMP
		subroutine nextyear(year,npop,currentpop, nalive)
			integer i, npop, nalive, year, j, dim_param
			type (person) currentpop(npop) , nextpop(npop)
			type (derivedvars) vars(nderivedvars)
			type (prob), allocatable:: trans_prob(:)
			double precision draw, mean
			integer s, a,  age
			double precision mx(2,nages), sx(2,nages),ex(2)	,ndem, px(2,nages)

			nalive = 0

			! copy record
			nextpop = currentpop

			!$OMP PARALLEL PRIVATE(vars, trans_prob) SHARED(currentpop,year, npop, ntransit, transit_models, nextpop)

      allocate(trans_prob(ntransit))
			do i = 1, ntransit, 1
				allocate(trans_prob(i)%value(transit_models(i)%neq))
			end do

			!$OMP DO
			do i = 1, npop, 1
				! get derived variables
				call getderivedvars(currentpop(i), vars,year)
				! get probabilities for transition models
				call getprobtrans(transit_models,ntransit,1,ntransit,vars, trans_prob,currentpop(i)%sex)
				! apply adjustments and trends on probabilities
				call transformprob(year, currentpop(i), trans_prob)
				! draw new states
				call newstates(year, currentpop(i), nextpop(i) , trans_prob)
				! compute use and costs for this year
			end do
			!$OMP END DO

            do i = 1, ntransit, 1
              deallocate(trans_prob(i)%value)
            end do
			deallocate(trans_prob)
			!$OMP END PARALLEL
			currentpop = nextpop
			nalive = sum(currentpop(:)%alive)
		end subroutine nextyear

		! function that that compute health cost, uses OPENMP
		subroutine updatehcare(year,npop,currentpop)
			integer i,j, npop, year
			type (person) currentpop(npop)
			type (derivedvars) vars(nderivedvars)
			type (prob), allocatable:: trans_prob(:)
			!$OMP PARALLEL PRIVATE(vars,trans_prob) SHARED(currentpop,year,nhcare, hcare_models, npop)
			allocate(trans_prob(nhcare))
            do i = 1, nhcare, 1
                allocate(trans_prob(i)%value(hcare_models(i)%neq))
            end do
			!$OMP DO
			do i = 1, npop, 1
				! get derived variables
				call getderivedvars(currentpop(i), vars,year)
				!write(*,*) 'here'
				call getprobtrans(hcare_models,nhcare,1,10,vars,trans_prob,currentpop(i)%sex)
				! compute use and costs for this year

				call gethcare(year, currentpop(i),1,10,trans_prob)

        call getderivedvars(currentpop(i), vars,year)

        call getprobtrans(hcare_models,nhcare,11,16,vars,trans_prob,currentpop(i)%sex)
                ! compute use and costs for this year
        call gethcare(year,currentpop(i),11,16,trans_prob)
			end do
			!$OMP END DO
			 do i = 1, nhcare, 1
              deallocate(trans_prob(i)%value)
            end do
            deallocate(trans_prob)
			!$OMP END PARALLEL

		end subroutine updatehcare

		! function that computes health care costs
		subroutine gethcare(year,resp,start_transit,stop_transit,trans_prob)
			integer year, i,ii, j, sexpos, k, s, yearpos
			integer start_transit, stop_transit
			type (person) resp

			double precision  draw,pp,ppp
			type (prob) trans_prob(nhcare)
			yearpos = (year - startyear)/gapyears + 1
			! compute indices for all utilization models
			do i = start_transit, stop_transit, 1
			    ii = i+1-start_transit

					! draw states
					if (hcare_models(i)%name .eq. 'hc_nights') then
					    !write(*,*) trans_prob(i)%value(1)
						if (int(trans_prob(ii)%value(1)) .GT. 365) then
							resp%hc_nights = 365
						else
							resp%hc_nights = int(trans_prob(ii)%value(1))
						end if

					else if (hcare_models(i)%name .eq. 'hc_specialist') then
							if (int(trans_prob(ii)%value(1)) .gt. 365) then
										resp%hc_specialist = int(365)
							else
										resp%hc_specialist = int(trans_prob(ii)%value(1))
							end if
					else if (hcare_models(i)%name .eq. 'hc_generalist') then
							if (int(trans_prob(ii)%value(1)) .gt. 365) then
									resp%hc_generalist = int(365)
							else
									resp%hc_generalist = int(trans_prob(ii)%value(1))
					  	end if

					else if (hcare_models(i)%name .eq. 'hc_hui') then
						resp%hc_hui = trans_prob(ii)%value(1)
					else if (hcare_models(i)%name .eq. 'hc_homecare') then
					    call random_number(draw)
					    pp = 0.0d0
					    ppp = 0.0d0
                        if (resp%ltc .ne. 2 ) then
                             resp%hc_homecare_f = 0
                             resp%hc_homecare_i = 0
                        else
                            pp =  trans_prob(ii)%value(1)
                            ppp = trans_prob(ii)%value(1)+trans_prob(ii)%value(2)
                            if (draw .le. pp) then
                                resp%hc_homecare_f = 1
                                resp%hc_homecare_i = 0
                            else if (draw .le. ppp) then
                                resp%hc_homecare_f = 0
                                resp%hc_homecare_i = 1
                            else
                                resp%hc_homecare_f = 1
                                resp%hc_homecare_i = 1
                            end if

                        end if

					else if (hcare_models(i)%name .eq. 'hc_homecare_sp') then
						call random_number(draw)
						if (resp%hc_homecare_i .ne. 1 ) then
							resp%hc_homecare_sp = 0
						else if (draw .lt. trans_prob(ii)%value(1)) then
							resp%hc_homecare_sp = 1
						else
							resp%hc_homecare_sp = 0
						end if


					else if (hcare_models(i)%name .eq. 'hc_drugs') then
						call random_number(draw)
						if (draw .lt. trans_prob(ii)%value(1)) then
							resp%hc_drugs = 1
						else
							resp%hc_drugs = 0
						end if
					end if



					if (hcare_models(i)%name .eq. 'cost_nights') then
						if (resp%hc_nights .gt. 0) then
							resp%cost_nights = trans_prob(ii)%value(1)*trendhcare(yearpos)%nights
							if (resp%cost_nights .lt. 0.0d0) then
								resp%cost_nights = 0.0d0
							end if
						else
							resp%cost_nights = 0.0d0
						end if
					end if
					if (hcare_models(i)%name .eq. 'cost_drugs') then
						if (resp%hc_drugs .gt. 0) then
							resp%cost_drugs = trans_prob(ii)%value(1)*trendhcare(yearpos)%drugs
						else
							resp%cost_drugs = 0.0d0
						end if

					else if (hcare_models(i)%name .eq. 'cost_specialist') then
						if (resp%hc_specialist .gt. 0) then

							resp%cost_specialist = trans_prob(ii)%value(1)*trendhcare(yearpos)%specialist
							if (resp%cost_specialist .lt. 0.0d0) then
								resp%cost_specialist = 0.0d0
							end if
						else
							resp%cost_specialist = 0.0d0
						end if

					else if (hcare_models(i)%name .eq. 'cost_generalist') then
						if (resp%hc_generalist .gt. 0) then
							resp%cost_generalist = trans_prob(ii)%value(1)*trendhcare(yearpos)%generalist
							if (resp%cost_generalist .lt. 0.0d0) then
								resp%cost_generalist = 0.0d0
							end if
						else
							resp%cost_generalist = 0.0d0
						end if

					else if (hcare_models(i)%name .eq. 'hours_homecare_f') then
						if (resp%hc_homecare_f .eq. 1) then
							resp%hours_homecare_f = exp(trans_prob(ii)%value(1)-0.5*1.1533**2)*52
						else
							resp%hours_homecare_f = 0.0d0
						end if
					else if (hcare_models(i)%name .eq. 'cost_homecare_f') then
                        if (resp%hc_homecare_f .eq. 1) then
                            resp%cost_homecare_f = trans_prob(ii)%value(1)*trendhcare(yearpos)%homecare
                        else
                            resp%cost_homecare_f = 0.0d0
                    end if

						else if (hcare_models(i)%name .eq. 'hours_homecare_i') then
						if (resp%hc_homecare_i .eq. 1) then
							resp%hours_homecare_i = trans_prob(ii)%value(1)*52
						else
							resp%hours_homecare_i = 0.0d0
						end if


						else if (hcare_models(i)%name .eq. 'hours_homecare_sp') then
						if (resp%hc_homecare_sp .eq. 1) then
							resp%hours_homecare_sp = trans_prob(ii)%value(1)*52
						else
							resp%hours_homecare_sp = 0.0d0
						end if

					else if (hcare_models(i)%name .eq. 'cost_nhome') then
						if (resp%ltc .eq. 3) then
							resp%cost_nhome = trans_prob(ii)%value(1)*trendhcare(yearpos)%nhome
						else
							resp%cost_nhome = 0.0d0
						end if
					end if
				!end if

			end do

		end subroutine

        ! function that computes derived variables
         subroutine getderivedvars(resp, vars,year)
        	integer i,year,period
        	type (person) resp
        	type (derivedvars) vars(nderivedvars)
        	vars(:)%label = derivedvarlabels(:)
        	period = (year - startyear)

        	do i = 1, nderivedvars, 1
						! age <60 spline
						if (vars(i)%label .eq. 'age45m') then
							vars(i)%value = splinem(resp%age, 45)
							! age >60 spline
						else if (vars(i)%label .eq. 'age45p') then
									vars(i)%value = splinep(resp%age, 45)

						! age <50 spline
						else  if (vars(i)%label .eq. 'age50m') then
        			vars(i)%value = splinem(resp%age, 50)
        		! age >50 spline
        		else if (vars(i)%label .eq. 'age50p') then
        			vars(i)%value = splinep(resp%age, 50)

						! age <60 spline
						else	if (vars(i)%label .eq. 'age60m') then
							vars(i)%value = splinem(resp%age, 60)
						! age >60 spline
						else if (vars(i)%label .eq. 'age60p') then
								vars(i)%value = splinep(resp%age, 60)

						! age <60 spline
						else	if (vars(i)%label .eq. 'age85m') then
							vars(i)%value = splinem(resp%age, 85)
						! age >60 spline
						else if (vars(i)%label .eq. 'age85p') then
								vars(i)%value = splinep(resp%age, 85)

						else if (vars(i)%label .eq. 'age6575') then
									vars(i)%value = min(max(resp%age-65,0),10)
        		! age > 50 <=65
        		else if (vars(i)%label .eq. 'age5065') then
                    vars(i)%value = min(max(resp%age-50,0),15)
                ! age > 65 <=75
                else if (vars(i)%label .eq. 'age6575') then
                    vars(i)%value = min(max(resp%age-65,0),10)
               ! age > 75
                else if (vars(i)%label .eq. 'age75p') then
                    vars(i)%value = splinep(resp%age, 75)
                else if (vars(i)%label .eq. 'agegr55p') then
                    if (resp%age .ge. 55) then
                        vars(i)%value = 1
                    else
                        vars(i)%value = 0
                    end if
                else if (vars(i)%label .eq. 'byear51') then
                    vars(i)%value = resp%byear + 65
        		! smoker
						else if (vars(i)%label .eq. 'year2008') then
							vars(i)%value = 1
        		else if (vars(i)%label .eq. 'smoker') then
        			vars(i)%value = dummy(resp%smoke,2)

        		! former smoker
        		else if (vars(i)%label .eq. 'former') then
        			vars(i)%value = dummy(resp%smoke,3)

        		! obese
         		else if (vars(i)%label .eq. 'obese') then
         			vars(i)%value = dummy(resp%bmi,2)

        		! very obese
         		else if (vars(i)%label .eq. 'veryobese') then
         			vars(i)%value = dummy(resp%bmi,3)

        		! sex (female)
         		else if (vars(i)%label .eq. 'sex') then
         			vars(i)%value = dummy(resp%sex,1)

        		! immigrant
         		else if (vars(i)%label .eq. 'imm') then
         			vars(i)%value = dummy(resp%imm,1)

        		! hs
         		else if (vars(i)%label .eq. 'hs') then
         			vars(i)%value = dummy(resp%educ,2)

        		! college
         		else if (vars(i)%label .eq. 'college') then
         			vars(i)%value = dummy(resp%educ,3)

        		! univ
         		else if (vars(i)%label .eq. 'univ') then
         			vars(i)%value = dummy(resp%educ,4)
        		! less then college
                else if (vars(i)%label .eq. 'hs_less') then
                    if (resp%educ .le. 2) then
                        vars(i)%value = 1
                    else
                     vars(i)%value = 0
                    end if
        		! quebec
         		else if (vars(i)%label .eq. 'quebec') then
         			vars(i)%value = dummy(resp%province,2)
						else if (vars(i)%label .eq. 's_quebec') then
									vars(i)%value = dummy(resp%province,2) * dummy(resp%sex,1)
					        		! ontario
         		else if (vars(i)%label .eq. 'ontario') then
         			vars(i)%value = dummy(resp%province,3)
						else if (vars(i)%label .eq. 's_ontario') then
							vars(i)%value = dummy(resp%province,3) * dummy(resp%sex,1)
        		! prairies
         		else if (vars(i)%label .eq. 'prairies') then
         			vars(i)%value = dummy(resp%province,4)
						else if (vars(i)%label .eq. 's_prairies') then
						 vars(i)%value = dummy(resp%province,4)* dummy(resp%sex,1)
        		! bc
         		else if (vars(i)%label .eq. 'bc') then
         			vars(i)%value = dummy(resp%province,5)
						else if (vars(i)%label .eq. 's_bc') then
						 vars(i)%value = dummy(resp%province,5) * dummy(resp%sex,1)
        		! diabe
         		else if (vars(i)%label .eq. 'diabe') then
         			vars(i)%value = dummy(resp%diabe,1)

        		! hibpe
         		else if (vars(i)%label .eq. 'hibpe') then
         			vars(i)%value = dummy(resp%hibpe,1)

        		! hearte
         		else if (vars(i)%label .eq. 'hearte') then
         			vars(i)%value = dummy(resp%hearte,1)

        		! lunge
         		else if (vars(i)%label .eq. 'lunge') then
         			vars(i)%value = dummy(resp%lunge,1)

        		! stroke
         		else if (vars(i)%label .eq. 'stroke') then
         			vars(i)%value = dummy(resp%stroke,1)

        		! mentae
         		else if (vars(i)%label .eq. 'mentae') then
         			vars(i)%value = dummy(resp%mentae,1)

        		! cancre
         		else if (vars(i)%label .eq. 'cancre') then
         			vars(i)%value = dummy(resp%cancre,1)


        		! all cardiovascular diseases (hibpe, hearte, stroke)
         		else if (vars(i)%label .eq. 'cardioe') then
         		if (resp%hearte .eq. 1 .OR. resp%hibpe .eq. 1 .OR. resp%stroke .eq. 1) then
        				vars(i)%value = 1.0d0
        			else
        				vars(i)%value = 0.0d0
        			end if

         		! iadl
         		else if (vars(i)%label .eq. 'iadl') then
        			if (resp%inv .eq. 3 .OR. resp%inv .ge. 5) then
        				vars(i)%value = 1.0d0
        			else
        				vars(i)%value = 0.0d0
        			end if

        		! adl
         		else if (vars(i)%label .eq. 'adl') then
        			if (resp%inv .eq. 4 .OR. resp%inv .eq. 6 .OR. resp%inv .eq. 7) then
        				vars(i)%value = 1.0d0
        			else
        				vars(i)%value = 0.0d0
        			end if

        		! cognitive
         		else if (vars(i)%label .eq. 'cognitive') then
        			if (resp%inv .eq. 2 .OR. resp%inv .eq. 5 .OR. resp%inv .eq. 7) then
        				vars(i)%value = 1.0d0
        			else
        				vars(i)%value = 0.0d0
        			end if

        		 !inv1
         		else if (vars(i)%label .eq. 'inv1') then
         			vars(i)%value = dummy(resp%inv,1)

        		 !inv2
         		else if (vars(i)%label .eq. 'inv2') then
         			vars(i)%value = dummy(resp%inv,2)

        		 !inv3
         		else if (vars(i)%label .eq. 'inv3') then
         			vars(i)%value = dummy(resp%inv,3)

        		 !inv4
         		else if (vars(i)%label .eq. 'inv4') then
         			vars(i)%value = dummy(resp%inv,4)

        		 !inv5
         		else if (vars(i)%label .eq. 'inv5') then
         			vars(i)%value = dummy(resp%inv,5)

        		 !inv6
         		else if (vars(i)%label .eq. 'inv6') then
         			vars(i)%value = dummy(resp%inv,6)

        		 !inv7
         		else if (vars(i)%label .eq. 'inv7') then
         			vars(i)%value = dummy(resp%inv,7)

         		 !ltc2
         		else if (vars(i)%label .eq. 'ltc2') then
         			vars(i)%value = dummy(resp%ltc,2)

         		 !ltc3
         		else if (vars(i)%label .eq. 'ltc3') then
         			vars(i)%value = dummy(resp%ltc,3)

        		! nhome
         		else if (vars(i)%label .eq. 'nhome') then
         			vars(i)%value = dummy(resp%ltc,3)

        		 !income1
         		else if (vars(i)%label .eq. 'income1') then
         			vars(i)%value = dummy(resp%income,1)

        		 !income 2
         		else if (vars(i)%label .eq. 'income2') then
         			vars(i)%value = dummy(resp%income,2)

        		 !income3
         		else if (vars(i)%label .eq. 'income3') then
         			vars(i)%value = dummy(resp%income,3)

        		 !income4
         		else if (vars(i)%label .eq. 'income4') then
         			vars(i)%value = dummy(resp%income,4)

        		 !income5
         		else if (vars(i)%label .eq. 'income5') then
         			vars(i)%value = dummy(resp%income,5)

        		! constant
         		else if (vars(i)%label .eq. 'constant') then
         			vars(i)%value = 1.0d0

        		! dead
         		else if (vars(i)%label .eq. 'dead') then
         			vars(i)%value = dummy(resp%alive,0)
        		    !hours homecare_f
                else if (vars(i)%label .eq. 'hours_homecare_f') then
                    vars(i)%value = dble(resp%hours_homecare_f)
        		! nights
        		else if (vars(i)%label .eq. 'nights') then
        			vars(i)%value = dble(resp%hc_nights)

        		else if (vars(i)%label .eq. 'nights2') then
        			vars(i)%value = dble(resp%hc_nights)**2

        		else if (vars(i)%label .eq. 'nights3') then
        			vars(i)%value = dble(resp%hc_nights)**3

        		else if (vars(i)%label .eq. 'nights4') then
        			vars(i)%value = dble(resp%hc_nights)**4

        		! specialist
        		else if (vars(i)%label .eq. 'specialist') then
        			vars(i)%value = dble(resp%hc_specialist)

        		else if (vars(i)%label .eq. 'specialist2') then
        			vars(i)%value = dble(resp%hc_specialist)**2

        		else if (vars(i)%label .eq. 'specialist3') then
        			vars(i)%value = dble(resp%hc_specialist)**3

        		else if (vars(i)%label .eq. 'specialist4') then
        			vars(i)%value = dble(resp%hc_specialist)**4

        		! generalist
        		else if (vars(i)%label .eq. 'generalist') then
        			vars(i)%value = dble(resp%hc_generalist)

        		else if (vars(i)%label .eq. 'generalist2') then
        			vars(i)%value = dble(resp%hc_generalist)**2

        		else if (vars(i)%label .eq. 'generalist3') then
        			vars(i)%value = dble(resp%hc_generalist)**3

        		else if (vars(i)%label .eq. 'generalist4') then
        			vars(i)%value = dble(resp%hc_generalist)**4

        		else if (vars(i)%label .eq. 'age65') then
        			if (resp%age .ge. 65) then
        				vars(i)%value = 1.0d0
        			else
        				vars(i)%value = 0.0d0
        			end if
        		else if (vars(i)%label .eq. 'age70') then
                    if (resp%age .ge. 70) then
                        vars(i)%value = 1.0d0
                    else
                        vars(i)%value = 0.0d0
                    end if
        		else if (vars(i)%label .eq. 'adliadl') then
        			if (resp%inv .ge. 2) then
        				vars(i)%value = 1.0d0
        			else
        				vars(i)%value = 0.0d0
        			end if
        		else if (vars(i)%label .eq. 'period') then
                    vars(i)%value = period
                else if (vars(i)%label .eq. 'year') then
                    vars(i)%value = year
                else if (vars(i)%label .eq. 'y_hsless') then
                    if (resp%educ .le. 2) then
                      vars(i)%value = resp%byear+65
                    else
                        vars(i)%value = 0
                    end if
                else if (vars(i)%label .eq. 'agegr_year') then
                    if (resp%age .ge. 55) then
                      vars(i)%value = resp%byear+65
                    else
                        vars(i)%value = 0
                    end if
                else if (vars(i)%label .eq. 'agegr_hsless') then
                    if (resp%educ .le. 2 .and. resp%age .ge. 55) then
                      vars(i)%value = 1
                    else
                        vars(i)%value = 0
                    end if
                else if (vars(i)%label .eq. 'agegr_hsless_year') then
                    if (resp%educ .le. 2 .and. resp%age .ge. 55) then
                      vars(i)%value = resp%byear+65
                    else
                        vars(i)%value = 0
                    end if

                end if
        	end do
        end subroutine getderivedvars

        !Subroutine to get the path of compas repository
        subroutine getpath(cwd)
            character*80 :: cwd,cwd_tmp,test
            integer c
            CALL getcwd(cwd_tmp)
            call GET_COMMAND_ARGUMENT(0,test)
            cwd_tmp= trim(cwd_tmp)//'/'//trim(test)
            c = INDEX(cwd_tmp,'compas/')
            if (c .ne. 0) then
                cwd = cwd_tmp(1:c+6)
                write(*,*) "Path : ", trim(cwd)
            else
                write(*,*) "Wrong path"
                write(*,*) "The actual path is : ",trim(cwd_tmp)
                cwd = "NULL"
            end if
        end subroutine getpath


      	!subroutine write stata dictionary
      	subroutine dictionary(r)
      	    integer r,i,status
      	    character*4 cr
      		character*80 row
			write(cr,'(i4)') r
      		open(3, file=adjustl(trim(path))//'do/stats/simpop.dct')
			open(4, file=adjustl(trim(path))//'output/'//adjustl(trim(scenario))//'/simpop-'//trim(adjustl(cr))//'.dct')
            row = 'infile dictionary using output/'//adjustl(trim(scenario))//'/simpop-'//trim(adjustl(cr))//'.dat {'
            write(4,*) row
            do
            	read(3,'(A)',iostat=status) row
            	if (status.ne.0) then
            		exit
            	end if
            	write(4,'(A)') row
            end do
            close(3)
        	close(4)
        end subroutine dictionary

        !  function that computes a spline (1st)
		double precision function splinem(x, upper)
			integer x, upper
			if (x .lt. upper) then
				splinem = dble(x)
			else
				splinem = dble(upper)
			end if
		end function splinem
		! function that computes a spline (2nd)
		double precision function splinep(x, lower)
			integer x, lower
			if (x .lt. lower) then
				splinep = 0.0d0
			else
				splinep = dble(x - lower)
			end if
		end function splinep
		! function that computes dummy variable
		double precision function dummy(x, value)
			integer x, value
			if (x .eq. value) then
				dummy = 1.0d0
			else
				dummy = 0.0d0
			end if
		end function dummy

		! quantile of normal distribution (uses dcdflib.a)
		double precision function qnorm(p)
		implicit none
		double precision :: p
		double precision :: mean
		double precision :: sd
		double precision :: bound, statuts
		mean = 0.0d0
		sd = 1.0d0
		call cdfnor(2, p, 1.0d0 - p, qnorm, mean, sd, statuts, bound)
		return
	  end function qnorm

	  ! cdf of normal distribution (uses dcdflib.a)
	  double precision function pnorm(x)
		implicit none
		double precision :: x
		double precision :: mean
		double precision :: sd
		double precision :: bound, statuts, q
		mean = 0.0d0
		sd = 1.0d0
		if (x .ge. 10.0d0) then
			pnorm = 1.0d0
		else if (x .le. -10.0d0) then
			pnorm = 0.0d0
		else
			call cdfnor(1, pnorm, q, x, mean, sd, statuts, bound)
		end if
		return
	  end function pnorm
	  !integer function


end module compas


! executable
program main
    use compas

    ! load settings
    call loadsettings
    ! run scenario
    call runpop

end program main

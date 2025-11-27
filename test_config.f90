program test_config

  use iso_fortran_env
  use output_config_module

  implicit none

  type(module_config) :: geophys_config, casa_config
  type(variable_config) :: tmp_var
  type(group_config) :: tmp_group

  integer :: i, j

  geophys_config = new_module("biogeophys", [&
    new_group("soil", [&
      new_variable("soil_moisture"),&
      new_variable("soil_temperature")&
      ]),&
    new_group("canopy", [&
      new_variable("sensible_heat_flux"),&
      new_variable("latent_heat_flux"),&
      new_variable("evaporation")&
      ]),&
    new_group("snow", [&
      new_variable("snow_temperature"),&
      new_variable("snow_density")&
      ])&
    ])

  casa_config = new_module("biogeochemistry", [&
    new_group("carbon", [&
      new_variable("GPP"),&
      new_variable("labile_carbon"),&
      new_variable("litter_carbon")&
      ]),&
    new_group("nitrogen", [&
      new_variable("leaf_nitrogen"),&
      new_variable("root_nitrogen")&
      ])&
    ])
 
  call process_config_file("biogeophys_config.conf", geophys_config)

  ! How to retrieve methods for a particular variable
  do i = 1, size(geophys_config%groups)
    tmp_group = geophys_config%groups(i)
    write(output_unit,*) "Group: ", tmp_group%group_name, ", active: ", tmp_group%active, ", options:", tmp_group%agg_methods
    do j = 1, size(geophys_config%groups(i)%variables)
      tmp_var = geophys_config%groups(i)%variables(j)
      write(output_unit,*)" Variable: ", tmp_var%var_name, "active: ", tmp_var%active, "options:", tmp_var%agg_methods
    end do
  end do

  call process_config_file("casa_config.conf", casa_config)
  
end program test_config

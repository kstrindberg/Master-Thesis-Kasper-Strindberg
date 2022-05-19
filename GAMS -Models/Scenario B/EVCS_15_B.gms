*Define scalars
scalar battery_cost 'Cost of Li-Oin battery NOK/kWh/4months' /220/
time 'length of one time period minutes' /5/
bigM 'Used for big M method' /10000000000/
ess_efficiency/0.96/
fixed_tax 'tax fee NOK/year' /1904/
energy_tax 'energy tax NOK/kWh' /0.01815/
fixed_fee 'fixed grid fee NOK/month' /340/
power_fee_winter 'fee per max kW per month NOV-MAR NOK/kW' /70/
power_fee_summer 'fee per max kW per month APR-OCT NOK/kW' /35/
energy_fee_summer 'fee per kWh APR-OCT NOK/kWh' /0.039/
energy_fee_winter 'fee per kWh NOV-MAR NOK/kWh' /0.07/
C_rate 'Battery C-rate' /1/
*electricity_prices 'assumed spotprice for electricity' /1/
grid_limit 'Desired max power from grid kW' /180/
;

*Define time index
set
m 'Time periods' /m0*m34541/
;

*Define the 8 chargers
set c/
    C1,
    C2,
    C3,
    C4,
    C5,
    C6,
    C7,
    C8
/;
*Define demand data 
parameter demand(m,c)/
$include demand_winter.tsv
*$include demand_winter_reduced27december.tsv
/;

* Define prices
parameter electricity_prices(m)/
$include Prices_Winter.tsv
/;

positive variables
* These are my decision variables
    var_ess_rated_power
    var_ESS_level(m)
    var_SOC_pen

    var_grid_power(m)
    var_grid_ex_power(m)
    var_grid_maxpower1
    var_grid_maxpower2
    var_grid_maxpower3
    var_grid_maxpower4
    var_grid_total_power(m)
    var_grid_energy(m)
    var_grid_ex_energy(m)
    var_grid_total_energy(m)    
    
    var_ESS_cost
    var_electricity_cost
    var_grid_tariff_winter
    var_energy_tax
    
    months

;

*binary variables
*    bin_charge(m)
*    bin_discharge(m)
*    bin_hier1(m)
*    bin_hier2(m)
*    bin_hier3(m)
*;


free variables
    var_system_cost
    var_ESS_size
    var_ESS_power(m)
; 


equations
*Objective Function
    eq_objective
    
* ESS Constraints
    eq_ESS_power_constraint1
    eq_ESS_power_constraint2
    eq_ESS_level
    eq_ESS_init_level
    eq_ESS_final_level
    eq_ESS_cost
    eq_ESS
    eq_ESS2
    eq_Crate
    eq_ESS_efficiency
    
* Costs
    eq_electricity_cost
    eq_grid_tariff_winter
    eq_energy_tax    

* Grid constraints
    eq_grid_balance
    eq_grid_energy
    eq_grid_ex_energy
    eq_grid_total_power
    eq_grid_max_power1
    eq_grid_max_power2
    eq_grid_max_power3
    eq_grid_max_power4

* Misc
    eq_months
    eq_cost_soc
;

*fix ess size
var_ESS_size.fx = 139.17;

* provide initial values
var_grid_power.up(m) = grid_limit;

* Count the amount of months the simulation runs
eq_months(m).. months =E= card(m)/8064;

*Objective Function
eq_objective.. var_system_cost =E= var_energy_tax + var_grid_tariff_winter + var_electricity_cost + var_ESS_cost
 + sum(m, var_SOC_pen(m)) * 0.2
;

* Electricity costs
eq_electricity_cost.. var_electricity_cost =E= sum(m, var_grid_energy(m) * electricity_prices(m) *3) + sum(m, var_grid_ex_energy(m) * electricity_prices(m) * 2 *3) 
;

*REMEMBER TO ACCOUNT FOR POWER_FEE FOR ALL MONTHS


* Grid Tariff (Nettleie)
eq_grid_tariff_winter.. var_grid_tariff_winter =E= sum(m, var_grid_energy(m) * energy_fee_winter) + sum(m, var_grid_ex_energy(m) * energy_fee_winter) + fixed_fee * months +
power_fee_winter * var_grid_maxpower1 +
power_fee_winter * var_grid_maxpower2 +
power_fee_winter * var_grid_maxpower3 +
power_fee_winter * var_grid_maxpower4
;

* Energy Tax
eq_energy_tax.. var_energy_tax =E= fixed_tax + energy_tax * sum(m, var_grid_energy(m));

*ESS cost
eq_ESS_cost(m).. var_ESS_cost =E= var_ESS_size * battery_cost
;


* converting grid power to energy
eq_grid_energy(m).. var_grid_energy(m) =E= var_grid_total_power(m) * (time/60);
eq_grid_ex_energy(m).. var_grid_ex_energy(m) =E= var_grid_ex_power(m) * (time/60);

eq_grid_total_power(m).. var_grid_total_power(m) =E= var_grid_power(m) + var_grid_ex_power(m);

* Grid balance
eq_grid_balance(m).. sum(c, demand(m,c)) =L= var_grid_power(m) + var_ESS_power(m) + var_grid_ex_power(m)
;

*penalty for low SOC
eq_cost_soc(m).. var_SOC_pen(m) =E= var_ESS_size - var_ESS_level(m)
;


*ESS Initial level
eq_ESS_init_level(m).. var_ESS_level("m0") =E= var_ESS_size
;

*ESS Final Level
eq_ESS_final_level(m).. var_ESS_level("m34541") =G= var_ESS_size * 0.8
;

*ESS level
eq_ESS_level(m) $ (not(ord(m)=1)).. var_ESS_level(m) =E= var_ESS_level(m-1) - var_ESS_power(m) * (time/60)
;

*ESS constraint
eq_ESS_power_constraint1(m).. var_ESS_power(m) =L= var_ess_rated_power
;

eq_ESS_power_constraint2(m).. var_ESS_power(m) =G= 0 - var_ess_rated_power
;


*ESS operational SOC
eq_ESS(m).. var_ESS_level(m) =L= var_ESS_size
;

eq_ESS2(m).. var_ESS_level(m) =G= var_ESS_size * 0.2
;

eq_ESS_efficiency.. sum(m, var_ESS_power(m)) =L= 0;

* ESS_size is at 80% SoC, therefore 1C is ESS size *1,25
eq_Crate.. var_ess_rated_power =L= C_rate * var_ESS_size * 1.25;

* Max power from grid
eq_grid_max_power1(m) $ (ord(m) <= 8634).. var_grid_maxpower1 =G= var_grid_total_power(m)
;
eq_grid_max_power2(m) $ (ord(m) >= 8635 and ord(m) <= 17269).. var_grid_maxpower2 =G= var_grid_total_power(m)
;
eq_grid_max_power3(m) $ (ord(m) >= 17270 and ord(m) <= 25903).. var_grid_maxpower3 =G= var_grid_total_power(m)
;
eq_grid_max_power4(m) $ (ord(m) >= 25904 and ord(m) <= 34541).. var_grid_maxpower4 =G= var_grid_total_power(m)
;


model optimal_generation /
                            all/;
 

                            
solve optimal_generation minimizing var_system_cost using MIP;

execute_unload 'EVCS_15_B1.gdx' var_grid_power, var_grid_energy, var_ESS_size, var_ESS_power, var_ess_rated_power,
var_ESS_level, var_electricity_cost, var_system_cost, var_ESS_cost, var_grid_tariff_winter, var_energy_tax,
months, var_grid_ex_power, var_grid_total_power, var_SOC_pen, var_grid_ex_energy,
var_grid_total_energy, var_grid_maxpower1,var_grid_maxpower2, var_grid_maxpower3, var_grid_maxpower4;
execute_unload "EVCS_15_B1"
execute "gdx2sqlite -i EVCS_15_B1.gdx -o EVCS_15_B1_results.db"


-- Seed: 15662161960401532916,2983771601630957889

use std.reflection.all;

entity pen is
  port (variable qik : inout physical_value_mirror_pt);
end pen;

architecture ykojqj of pen is
  
begin
  
end ykojqj;

entity bltyqaa is
  port (ajvtzyd : inout severity_level; jaj : linkage real);
end bltyqaa;

use std.reflection.all;

architecture n of bltyqaa is
  shared variable kzrq : physical_value_mirror_pt;
  shared variable rzmdxeui : physical_value_mirror_pt;
begin
  wqdpnkpl : entity work.pen
    port map (qik => rzmdxeui);
  yzujks : entity work.pen
    port map (qik => kzrq);
  
  -- Single-driven assignments
  ajvtzyd <= FAILURE;
end n;



-- Seed after: 2071937829232290428,2983771601630957889

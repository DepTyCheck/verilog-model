-- Seed: 8544354534714385470,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity sorgihyoz is
  port (cf : inout std_logic_vector(2 to 0));
end sorgihyoz;

architecture xnnl of sorgihyoz is
  
begin
  
end xnnl;

entity weyhifyihs is
  port (vdddqffpb : inout time);
end weyhifyihs;

library ieee;
use ieee.std_logic_1164.all;

architecture ykfhjyy of weyhifyihs is
  signal yqprkeri : std_logic_vector(2 to 0);
  signal g : std_logic_vector(2 to 0);
  signal xxalmrup : std_logic_vector(2 to 0);
begin
  yzhfj : entity work.sorgihyoz
    port map (cf => xxalmrup);
  ojo : entity work.sorgihyoz
    port map (cf => g);
  tnpsq : entity work.sorgihyoz
    port map (cf => yqprkeri);
  
  -- Single-driven assignments
  vdddqffpb <= vdddqffpb;
  
  -- Multi-driven assignments
  xxalmrup <= (others => '0');
  xxalmrup <= (others => '0');
  g <= xxalmrup;
end ykfhjyy;



-- Seed after: 10607267296286335772,10754487200446211253

-- Seed: 11948136825422137306,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity ioh is
  port (yfs : inout real_vector(2 to 1); pugrwosfy : linkage std_logic_vector(2 to 4));
end ioh;

architecture df of ioh is
  
begin
  -- Single-driven assignments
  yfs <= (others => 0.0);
end df;

entity tum is
  port (xyexw : inout severity_level; vvxg : linkage string(3 to 2));
end tum;

library ieee;
use ieee.std_logic_1164.all;

architecture dxbdmj of tum is
  signal pmdi : std_logic_vector(2 to 4);
  signal ekf : real_vector(2 to 1);
  signal pkc : std_logic_vector(2 to 4);
  signal liqj : real_vector(2 to 1);
  signal sutqwzxk : std_logic_vector(2 to 4);
  signal mtlteudnpe : real_vector(2 to 1);
  signal dktfcy : std_logic_vector(2 to 4);
  signal ljrmcl : real_vector(2 to 1);
begin
  um : entity work.ioh
    port map (yfs => ljrmcl, pugrwosfy => dktfcy);
  ahlmoi : entity work.ioh
    port map (yfs => mtlteudnpe, pugrwosfy => sutqwzxk);
  btyube : entity work.ioh
    port map (yfs => liqj, pugrwosfy => pkc);
  miqtzbb : entity work.ioh
    port map (yfs => ekf, pugrwosfy => pmdi);
  
  -- Single-driven assignments
  xyexw <= ERROR;
  
  -- Multi-driven assignments
  dktfcy <= dktfcy;
end dxbdmj;



-- Seed after: 18254519993466509570,3400751927341804175

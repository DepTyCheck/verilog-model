-- Seed: 15647306444455981521,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity kfgdcmout is
  port (lszgtoluuh : inout time; lqdcv : out real; nxxoon : buffer std_logic_vector(2 downto 1); smozj : inout time);
end kfgdcmout;

architecture qb of kfgdcmout is
  
begin
  -- Multi-driven assignments
  nxxoon <= "-1";
end qb;

entity eoklu is
  port (lgs : in string(3 downto 3));
end eoklu;

library ieee;
use ieee.std_logic_1164.all;

architecture h of eoklu is
  signal tbytkg : time;
  signal vuk : std_logic_vector(2 downto 1);
  signal hmpglr : real;
  signal ouiwxgkyn : time;
  signal yof : time;
  signal agzhmxy : std_logic_vector(2 downto 1);
  signal ofzqzsnc : real;
  signal z : time;
  signal hkkwaoyqp : time;
  signal fcnervp : std_logic_vector(2 downto 1);
  signal ytutaspwe : real;
  signal rilduj : time;
begin
  wtpqyohqd : entity work.kfgdcmout
    port map (lszgtoluuh => rilduj, lqdcv => ytutaspwe, nxxoon => fcnervp, smozj => hkkwaoyqp);
  hbfxaaitiz : entity work.kfgdcmout
    port map (lszgtoluuh => z, lqdcv => ofzqzsnc, nxxoon => agzhmxy, smozj => yof);
  cprjhq : entity work.kfgdcmout
    port map (lszgtoluuh => ouiwxgkyn, lqdcv => hmpglr, nxxoon => vuk, smozj => tbytkg);
  
  -- Multi-driven assignments
  fcnervp <= "-Z";
  vuk <= ('H', 'L');
  fcnervp <= ('W', '0');
  fcnervp <= "HX";
end h;



-- Seed after: 3413857101464914863,14629254427735353553

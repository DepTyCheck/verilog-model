-- Seed: 17080206706910478348,6882842853887419669

entity udrwayrw is
  port (jcjirai : in time; emfb : in integer);
end udrwayrw;

architecture deco of udrwayrw is
  
begin
  
end deco;

library ieee;
use ieee.std_logic_1164.all;

entity ntpnjegxl is
  port (jlkjexuoe : inout time; orz : inout std_logic_vector(3 to 1); zryeawb : in integer);
end ntpnjegxl;

architecture knbxtxso of ntpnjegxl is
  signal u : integer;
  signal fqvlm : time;
  signal ghxueq : time;
  signal zrgkmqd : time;
begin
  wfy : entity work.udrwayrw
    port map (jcjirai => zrgkmqd, emfb => zryeawb);
  qcfe : entity work.udrwayrw
    port map (jcjirai => ghxueq, emfb => zryeawb);
  g : entity work.udrwayrw
    port map (jcjirai => fqvlm, emfb => u);
  
  -- Single-driven assignments
  u <= 2#1_0_1_0#;
  fqvlm <= 16#A_8_B_9.D8032# ns;
  zrgkmqd <= 1 hr;
  jlkjexuoe <= 16#D_E_B.F_F_9_E_5# ps;
  ghxueq <= 1_0_1_0_1 ms;
  
  -- Multi-driven assignments
  orz <= (others => '0');
end knbxtxso;



-- Seed after: 14749966878117016864,6882842853887419669

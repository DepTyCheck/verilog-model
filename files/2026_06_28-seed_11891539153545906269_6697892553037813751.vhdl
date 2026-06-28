-- Seed: 11891539153545906269,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (ftaqvzzc : in time; yhtzazoxbu : out real; gyuc : inout std_logic_vector(0 to 4));
end a;

architecture aej of a is
  
begin
  -- Single-driven assignments
  yhtzazoxbu <= 1.1_3_3;
end aej;

library ieee;
use ieee.std_logic_1164.all;

entity wpfknceii is
  port (qokzqtk : inout std_logic_vector(3 downto 0); cg : out time; hbngdoei : in time);
end wpfknceii;

library ieee;
use ieee.std_logic_1164.all;

architecture ydzziifpo of wpfknceii is
  signal abd : std_logic_vector(0 to 4);
  signal lhxudve : real;
  signal tika : std_logic_vector(0 to 4);
  signal bh : real;
begin
  qdjlsip : entity work.a
    port map (ftaqvzzc => cg, yhtzazoxbu => bh, gyuc => tika);
  oz : entity work.a
    port map (ftaqvzzc => hbngdoei, yhtzazoxbu => lhxudve, gyuc => abd);
  
  -- Single-driven assignments
  cg <= 3_4_1_2_3.4004 us;
  
  -- Multi-driven assignments
  qokzqtk <= "-WHU";
  qokzqtk <= "W00L";
end ydzziifpo;



-- Seed after: 15511807865513999787,6697892553037813751

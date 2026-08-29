-- Seed: 5420501016475896727,10463297573877745897

library ieee;
use ieee.std_logic_1164.all;

entity bjimidnmsc is
  port (zvlivrks : inout std_logic_vector(1 to 0); pw : inout integer);
end bjimidnmsc;

architecture eeakzphxxm of bjimidnmsc is
  
begin
  -- Single-driven assignments
  pw <= 16#6_E_E#;
  
  -- Multi-driven assignments
  zvlivrks <= "";
  zvlivrks <= (others => '0');
  zvlivrks <= zvlivrks;
end eeakzphxxm;

entity zwbcgxw is
  port (di : in boolean; m : out bit_vector(2 to 4); dukxlp : linkage severity_level; fnbitgd : out boolean_vector(1 to 1));
end zwbcgxw;

library ieee;
use ieee.std_logic_1164.all;

architecture wb of zwbcgxw is
  signal huplrj : integer;
  signal glx : integer;
  signal wkurnbxabg : std_logic_vector(1 to 0);
begin
  qrljcn : entity work.bjimidnmsc
    port map (zvlivrks => wkurnbxabg, pw => glx);
  cocpgzd : entity work.bjimidnmsc
    port map (zvlivrks => wkurnbxabg, pw => huplrj);
  
  -- Single-driven assignments
  fnbitgd <= (others => TRUE);
  
  -- Multi-driven assignments
  wkurnbxabg <= (others => '0');
  wkurnbxabg <= wkurnbxabg;
end wb;



-- Seed after: 5435901160498614841,10463297573877745897

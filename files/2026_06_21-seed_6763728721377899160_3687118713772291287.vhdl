-- Seed: 6763728721377899160,3687118713772291287

entity w is
  port (pi : in time; kbu : out time; tj : linkage string(5 downto 2));
end w;

architecture npoar of w is
  
begin
  
end npoar;

library ieee;
use ieee.std_logic_1164.all;

entity pvir is
  port (rwpu : linkage std_logic_vector(1 downto 0); qmkoplpi : linkage std_logic; svtsfsq : inout bit);
end pvir;

architecture bxqnm of pvir is
  signal mhnkisoml : string(5 downto 2);
  signal tlgprqvfj : time;
  signal lrivjk : time;
  signal jnkmpopb : string(5 downto 2);
  signal gurvcn : time;
  signal amao : time;
begin
  wzxcpypzu : entity work.w
    port map (pi => amao, kbu => gurvcn, tj => jnkmpopb);
  wmweoelqnv : entity work.w
    port map (pi => lrivjk, kbu => tlgprqvfj, tj => mhnkisoml);
  
  -- Single-driven assignments
  svtsfsq <= '1';
  lrivjk <= 16#E_4_5_D_4.4_B_5_8_7# ns;
  amao <= 8#5.0# ns;
end bxqnm;

library ieee;
use ieee.std_logic_1164.all;

entity qksvrtceg is
  port (rflbrp : linkage std_logic_vector(1 to 2); psaeiy : buffer real; rozjk : inout integer);
end qksvrtceg;

library ieee;
use ieee.std_logic_1164.all;

architecture gjj of qksvrtceg is
  signal tdvzyu : bit;
  signal ecb : std_logic;
  signal jlq : std_logic_vector(1 downto 0);
  signal bvwfph : bit;
  signal uadiprri : std_logic;
  signal qgylmtfz : std_logic_vector(1 downto 0);
  signal seymzc : string(5 downto 2);
  signal evvdoeh : time;
  signal dtunxzkmu : time;
begin
  jdgmppla : entity work.w
    port map (pi => dtunxzkmu, kbu => evvdoeh, tj => seymzc);
  slczj : entity work.pvir
    port map (rwpu => qgylmtfz, qmkoplpi => uadiprri, svtsfsq => bvwfph);
  a : entity work.pvir
    port map (rwpu => jlq, qmkoplpi => ecb, svtsfsq => tdvzyu);
  
  -- Single-driven assignments
  rozjk <= 8#0#;
  psaeiy <= 16#A_C_5.2AB#;
end gjj;



-- Seed after: 16070812542603070289,3687118713772291287

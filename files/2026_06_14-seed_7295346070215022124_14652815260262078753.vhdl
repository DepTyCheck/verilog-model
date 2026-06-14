-- Seed: 7295346070215022124,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity nqjhcmsjz is
  port (vj : inout std_logic_vector(3 to 0); rz : buffer std_logic; rezxo : inout time);
end nqjhcmsjz;

architecture nifbvhg of nqjhcmsjz is
  
begin
  -- Single-driven assignments
  rezxo <= 8#0_5_4_2_0# fs;
  
  -- Multi-driven assignments
  rz <= '0';
  vj <= "";
  rz <= 'Z';
end nifbvhg;

entity laiqkw is
  port (itewid : buffer boolean_vector(3 downto 0));
end laiqkw;

library ieee;
use ieee.std_logic_1164.all;

architecture i of laiqkw is
  signal mpoojkxec : time;
  signal ioudb : std_logic;
  signal tsawhu : std_logic_vector(3 to 0);
  signal udhppt : time;
  signal vogchc : time;
  signal odbxxjsju : std_logic;
  signal rehwhv : time;
  signal dhltyfkzc : std_logic;
  signal ewpjlgge : std_logic_vector(3 to 0);
begin
  jaqe : entity work.nqjhcmsjz
    port map (vj => ewpjlgge, rz => dhltyfkzc, rezxo => rehwhv);
  wrn : entity work.nqjhcmsjz
    port map (vj => ewpjlgge, rz => odbxxjsju, rezxo => vogchc);
  eehwjffq : entity work.nqjhcmsjz
    port map (vj => ewpjlgge, rz => dhltyfkzc, rezxo => udhppt);
  eoxw : entity work.nqjhcmsjz
    port map (vj => tsawhu, rz => ioudb, rezxo => mpoojkxec);
  
  -- Single-driven assignments
  itewid <= (FALSE, TRUE, FALSE, TRUE);
end i;



-- Seed after: 16777136039226455015,14652815260262078753

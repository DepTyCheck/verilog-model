-- Seed: 17339408685784560924,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity ngacni is
  port (cpnecrnj : linkage integer; bnvgjsymua : out std_logic_vector(1 downto 1); znj : linkage boolean_vector(3 to 0));
end ngacni;

architecture igw of ngacni is
  
begin
  -- Multi-driven assignments
  bnvgjsymua <= (others => '-');
  bnvgjsymua <= (others => 'H');
  bnvgjsymua <= (others => 'L');
end igw;

entity rlsplayjon is
  port (sgeobf : out time);
end rlsplayjon;

library ieee;
use ieee.std_logic_1164.all;

architecture rollrwcitr of rlsplayjon is
  signal kvplcgfj : boolean_vector(3 to 0);
  signal kgef : std_logic_vector(1 downto 1);
  signal uz : integer;
  signal rofqvcnzrt : boolean_vector(3 to 0);
  signal fwwioi : integer;
  signal qezuekjgrk : boolean_vector(3 to 0);
  signal ovytxnyetg : std_logic_vector(1 downto 1);
  signal mihpzxh : integer;
begin
  pjzahho : entity work.ngacni
    port map (cpnecrnj => mihpzxh, bnvgjsymua => ovytxnyetg, znj => qezuekjgrk);
  tpli : entity work.ngacni
    port map (cpnecrnj => fwwioi, bnvgjsymua => ovytxnyetg, znj => rofqvcnzrt);
  sa : entity work.ngacni
    port map (cpnecrnj => uz, bnvgjsymua => kgef, znj => kvplcgfj);
  
  -- Single-driven assignments
  sgeobf <= 2#1_0.1_0_0_1_0# ns;
end rollrwcitr;



-- Seed after: 14416901758134850815,2287986438313874379

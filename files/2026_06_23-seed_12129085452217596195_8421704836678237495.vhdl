-- Seed: 12129085452217596195,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity fkr is
  port (aroeqiwdg : buffer std_logic; cphczv : in real_vector(4 to 2); gqef : linkage time_vector(2 to 3); d : linkage time);
end fkr;

architecture lukrkcgcn of fkr is
  
begin
  -- Multi-driven assignments
  aroeqiwdg <= 'H';
  aroeqiwdg <= '1';
  aroeqiwdg <= 'L';
  aroeqiwdg <= '-';
end lukrkcgcn;

entity hrhhpgw is
  port (fcxbbkrxq : buffer bit_vector(4 downto 1));
end hrhhpgw;

library ieee;
use ieee.std_logic_1164.all;

architecture bsiayfphzw of hrhhpgw is
  signal j : time;
  signal etyewv : time_vector(2 to 3);
  signal nrfu : std_logic;
  signal al : time;
  signal gaysu : time_vector(2 to 3);
  signal blbvgzye : std_logic;
  signal mihchop : time;
  signal qbl : time_vector(2 to 3);
  signal nnez : real_vector(4 to 2);
  signal ovpnvhgmx : std_logic;
begin
  bvlbshb : entity work.fkr
    port map (aroeqiwdg => ovpnvhgmx, cphczv => nnez, gqef => qbl, d => mihchop);
  fcrerg : entity work.fkr
    port map (aroeqiwdg => blbvgzye, cphczv => nnez, gqef => gaysu, d => al);
  alugmvsb : entity work.fkr
    port map (aroeqiwdg => nrfu, cphczv => nnez, gqef => etyewv, d => j);
  
  -- Single-driven assignments
  fcxbbkrxq <= ('0', '0', '1', '1');
  nnez <= (others => 0.0);
  
  -- Multi-driven assignments
  ovpnvhgmx <= '0';
  ovpnvhgmx <= '1';
  ovpnvhgmx <= '1';
  ovpnvhgmx <= 'L';
end bsiayfphzw;

entity dnr is
  port (z : in time; ecv : linkage real);
end dnr;

architecture yub of dnr is
  
begin
  
end yub;

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (ylo : buffer boolean_vector(2 to 2); bnbzvoebvq : buffer std_logic);
end j;

library ieee;
use ieee.std_logic_1164.all;

architecture sr of j is
  signal a : bit_vector(4 downto 1);
  signal e : time;
  signal yygcynrzlj : time_vector(2 to 3);
  signal mztrjp : std_logic;
  signal bjiqmqte : time;
  signal vvpesh : time_vector(2 to 3);
  signal cjbmae : real_vector(4 to 2);
begin
  wlk : entity work.fkr
    port map (aroeqiwdg => bnbzvoebvq, cphczv => cjbmae, gqef => vvpesh, d => bjiqmqte);
  io : entity work.fkr
    port map (aroeqiwdg => mztrjp, cphczv => cjbmae, gqef => yygcynrzlj, d => e);
  wti : entity work.hrhhpgw
    port map (fcxbbkrxq => a);
  
  -- Multi-driven assignments
  bnbzvoebvq <= 'L';
  mztrjp <= 'W';
end sr;



-- Seed after: 16350836091359168576,8421704836678237495

-- Seed: 13188089676750158920,6298042963991371649



entity bv is
  port (n : in real; lunyscsvpj : linkage time_vector(2 to 1); nu : buffer integer_vector(0 downto 2));
end bv;



architecture ctlvcip of bv is
  
begin
  
end ctlvcip;

library ieee;
use ieee.std_logic_1164.all;

entity pxp is
  port (uorfumajzv : buffer boolean_vector(1 downto 4); ztyprsm : out time_vector(0 to 4); uzadcvsdd : out std_logic);
end pxp;



architecture nwql of pxp is
  signal yw : integer_vector(0 downto 2);
  signal efjsfhmdww : real;
  signal qnw : integer_vector(0 downto 2);
  signal zna : time_vector(2 to 1);
  signal hrzpqos : real;
begin
  sneoflmvy : entity work.bv
    port map (n => hrzpqos, lunyscsvpj => zna, nu => qnw);
  ceic : entity work.bv
    port map (n => efjsfhmdww, lunyscsvpj => zna, nu => yw);
end nwql;

library ieee;
use ieee.std_logic_1164.all;

entity etufouvmo is
  port (zrndcgmr : in time; lwodmhp : linkage std_logic);
end etufouvmo;

library ieee;
use ieee.std_logic_1164.all;

architecture ysvjpklc of etufouvmo is
  signal ofulngp : std_logic;
  signal hnq : time_vector(0 to 4);
  signal ibf : boolean_vector(1 downto 4);
  signal g : integer_vector(0 downto 2);
  signal ot : integer_vector(0 downto 2);
  signal dcllht : time_vector(2 to 1);
  signal pvuul : real;
begin
  rgmirzvvmv : entity work.bv
    port map (n => pvuul, lunyscsvpj => dcllht, nu => ot);
  ljmwgajusp : entity work.bv
    port map (n => pvuul, lunyscsvpj => dcllht, nu => g);
  jxczxk : entity work.pxp
    port map (uorfumajzv => ibf, ztyprsm => hnq, uzadcvsdd => ofulngp);
end ysvjpklc;



-- Seed after: 7441548879973130158,6298042963991371649

-- Seed: 4412266643266046985,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity wthizh is
  port (dvwhl : in time_vector(2 downto 3); poandrgdc : inout real_vector(4 downto 2); ceu : buffer std_logic_vector(4 downto 3));
end wthizh;

architecture fnjxcf of wthizh is
  
begin
  -- Single-driven assignments
  poandrgdc <= poandrgdc;
  
  -- Multi-driven assignments
  ceu <= ceu;
  ceu <= ceu;
end fnjxcf;

entity xtbseccfqu is
  port (mbgxpctqgq : out real);
end xtbseccfqu;

architecture kwvs of xtbseccfqu is
  
begin
  -- Single-driven assignments
  mbgxpctqgq <= mbgxpctqgq;
end kwvs;

library ieee;
use ieee.std_logic_1164.all;

entity orlbiis is
  port (fw : in std_logic_vector(3 to 1); bdvnv : inout boolean);
end orlbiis;

library ieee;
use ieee.std_logic_1164.all;

architecture wnc of orlbiis is
  signal mwtscgg : std_logic_vector(4 downto 3);
  signal q : real_vector(4 downto 2);
  signal plm : std_logic_vector(4 downto 3);
  signal wegvzvw : real_vector(4 downto 2);
  signal lve : time_vector(2 downto 3);
begin
  baqmngt : entity work.wthizh
    port map (dvwhl => lve, poandrgdc => wegvzvw, ceu => plm);
  mhu : entity work.wthizh
    port map (dvwhl => lve, poandrgdc => q, ceu => mwtscgg);
  
  -- Multi-driven assignments
  plm <= "UH";
  plm <= ('X', 'U');
end wnc;

entity gamifydvaq is
  port (lylhw : out time);
end gamifydvaq;

library ieee;
use ieee.std_logic_1164.all;

architecture azias of gamifydvaq is
  signal oztmivqdhw : std_logic_vector(4 downto 3);
  signal pfxgrcm : real_vector(4 downto 2);
  signal ofy : boolean;
  signal wmp : std_logic_vector(3 to 1);
  signal mwqjxu : std_logic_vector(4 downto 3);
  signal fqqxohqnv : real_vector(4 downto 2);
  signal ucdjlvlt : time_vector(2 downto 3);
begin
  tuhxkkcefr : entity work.wthizh
    port map (dvwhl => ucdjlvlt, poandrgdc => fqqxohqnv, ceu => mwqjxu);
  etpwgzx : entity work.orlbiis
    port map (fw => wmp, bdvnv => ofy);
  zyqhawdalk : entity work.wthizh
    port map (dvwhl => ucdjlvlt, poandrgdc => pfxgrcm, ceu => oztmivqdhw);
  
  -- Single-driven assignments
  lylhw <= 2_2_1_0_0 ps;
  ucdjlvlt <= (others => 0 ns);
  
  -- Multi-driven assignments
  wmp <= wmp;
  mwqjxu <= "HL";
end azias;



-- Seed after: 5817402577160036562,13857275728440271305

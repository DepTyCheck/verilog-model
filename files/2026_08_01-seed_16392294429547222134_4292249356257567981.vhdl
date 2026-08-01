-- Seed: 16392294429547222134,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity otveywj is
  port (ktr : in std_logic_vector(2 downto 3); gqvj : inout bit_vector(2 to 4); kjmpw : out character; ynsvqma : inout time);
end otveywj;

architecture twwycf of otveywj is
  
begin
  
end twwycf;

library ieee;
use ieee.std_logic_1164.all;

entity nfbxyospvg is
  port (vnmlgaod : buffer std_logic; prthrtfsb : buffer time);
end nfbxyospvg;

library ieee;
use ieee.std_logic_1164.all;

architecture t of nfbxyospvg is
  signal grogduwjzv : character;
  signal aos : bit_vector(2 to 4);
  signal ikjj : time;
  signal hxcrpseav : character;
  signal xrao : bit_vector(2 to 4);
  signal txmefesmz : std_logic_vector(2 downto 3);
  signal omtdsrinum : time;
  signal owsfpjbj : character;
  signal gnf : bit_vector(2 to 4);
  signal bkzbwawzzq : time;
  signal xlczdhuubh : character;
  signal pbairyeq : bit_vector(2 to 4);
  signal aax : std_logic_vector(2 downto 3);
begin
  rigjgtvsca : entity work.otveywj
    port map (ktr => aax, gqvj => pbairyeq, kjmpw => xlczdhuubh, ynsvqma => bkzbwawzzq);
  y : entity work.otveywj
    port map (ktr => aax, gqvj => gnf, kjmpw => owsfpjbj, ynsvqma => omtdsrinum);
  mztca : entity work.otveywj
    port map (ktr => txmefesmz, gqvj => xrao, kjmpw => hxcrpseav, ynsvqma => ikjj);
  xlihejc : entity work.otveywj
    port map (ktr => aax, gqvj => aos, kjmpw => grogduwjzv, ynsvqma => prthrtfsb);
  
  -- Multi-driven assignments
  vnmlgaod <= 'W';
  vnmlgaod <= vnmlgaod;
  txmefesmz <= aax;
  vnmlgaod <= '-';
end t;



-- Seed after: 18357471241021253824,4292249356257567981

-- Seed: 15727321546352314991,9125939553767483053



entity nhehphlqdb is
  port (d : buffer bit_vector(4 downto 0));
end nhehphlqdb;



architecture pguj of nhehphlqdb is
  
begin
  
end pguj;



entity s is
  port (lrieayfj : inout boolean_vector(1 downto 1); pvjxnh : inout bit);
end s;



architecture vkeyezqty of s is
  signal jlxw : bit_vector(4 downto 0);
  signal rbmhywso : bit_vector(4 downto 0);
  signal gsn : bit_vector(4 downto 0);
  signal vason : bit_vector(4 downto 0);
begin
  gn : entity work.nhehphlqdb
    port map (d => vason);
  wagzdc : entity work.nhehphlqdb
    port map (d => gsn);
  qiwb : entity work.nhehphlqdb
    port map (d => rbmhywso);
  hxt : entity work.nhehphlqdb
    port map (d => jlxw);
end vkeyezqty;



entity eybqrajkec is
  port (t : linkage time);
end eybqrajkec;



architecture rmztnxvzx of eybqrajkec is
  signal qzjmyctn : bit;
  signal ozx : boolean_vector(1 downto 1);
begin
  ut : entity work.s
    port map (lrieayfj => ozx, pvjxnh => qzjmyctn);
end rmztnxvzx;

library ieee;
use ieee.std_logic_1164.all;

entity bfvkr is
  port (inefpxkb : in std_logic_vector(3 to 1));
end bfvkr;



architecture erao of bfvkr is
  signal ti : bit_vector(4 downto 0);
  signal bsa : bit;
  signal xfyzyeyvc : boolean_vector(1 downto 1);
  signal wksifj : bit_vector(4 downto 0);
  signal ohninc : bit_vector(4 downto 0);
begin
  lcfj : entity work.nhehphlqdb
    port map (d => ohninc);
  ygtri : entity work.nhehphlqdb
    port map (d => wksifj);
  ylmcvuwdi : entity work.s
    port map (lrieayfj => xfyzyeyvc, pvjxnh => bsa);
  kyui : entity work.nhehphlqdb
    port map (d => ti);
end erao;



-- Seed after: 12812951978459858394,9125939553767483053

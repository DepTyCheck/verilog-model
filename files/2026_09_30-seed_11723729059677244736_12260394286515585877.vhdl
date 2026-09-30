-- Seed: 11723729059677244736,12260394286515585877

entity ubqa is
  port (kjv : buffer string(5 downto 3));
end ubqa;

architecture bca of ubqa is
  
begin
  -- Single-driven assignments
  kjv <= kjv;
end bca;

entity mus is
  port (avtoesdzio : linkage real; stmepdsfi : linkage integer; zmqavzcau : in time);
end mus;

architecture slysclhik of mus is
  signal jjgkwnxte : string(5 downto 3);
  signal a : string(5 downto 3);
  signal nbuugvukjr : string(5 downto 3);
begin
  khzrv : entity work.ubqa
    port map (kjv => nbuugvukjr);
  iusjlhq : entity work.ubqa
    port map (kjv => a);
  udvikm : entity work.ubqa
    port map (kjv => jjgkwnxte);
end slysclhik;

library ieee;
use ieee.std_logic_1164.all;

entity ubnkessqz is
  port (puzqpm : in std_logic_vector(2 downto 3); f : linkage severity_level; io : linkage integer);
end ubnkessqz;

architecture snwkxqhl of ubnkessqz is
  signal dspwczwvsm : time;
  signal hzgm : real;
  signal otudlpj : string(5 downto 3);
begin
  ekk : entity work.ubqa
    port map (kjv => otudlpj);
  weogvbv : entity work.mus
    port map (avtoesdzio => hzgm, stmepdsfi => io, zmqavzcau => dspwczwvsm);
  
  -- Single-driven assignments
  dspwczwvsm <= 8#435# ns;
end snwkxqhl;



-- Seed after: 8335601604917847570,12260394286515585877

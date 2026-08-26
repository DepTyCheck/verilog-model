-- Seed: 13790178187836941964,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity chjy is
  port (dvwmlgvypc : inout std_logic; cyb : in std_logic_vector(1 to 2); uiehyhh : out time_vector(2 downto 0));
end chjy;

architecture r of chjy is
  
begin
  
end r;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (fzvis : out std_logic; hnuio : inout integer; tkgzef : buffer severity_level);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture grgzmpvprc of a is
  signal vcnvgvf : time_vector(2 downto 0);
  signal y : std_logic_vector(1 to 2);
  signal rmdrnwisk : time_vector(2 downto 0);
  signal uybfzqzs : time_vector(2 downto 0);
  signal ooaxovcsd : std_logic_vector(1 to 2);
  signal smf : time_vector(2 downto 0);
  signal tlvt : std_logic_vector(1 to 2);
begin
  tdm : entity work.chjy
    port map (dvwmlgvypc => fzvis, cyb => tlvt, uiehyhh => smf);
  iboppwc : entity work.chjy
    port map (dvwmlgvypc => fzvis, cyb => ooaxovcsd, uiehyhh => uybfzqzs);
  dyoektdyf : entity work.chjy
    port map (dvwmlgvypc => fzvis, cyb => tlvt, uiehyhh => rmdrnwisk);
  labaidkwfm : entity work.chjy
    port map (dvwmlgvypc => fzvis, cyb => y, uiehyhh => vcnvgvf);
  
  -- Single-driven assignments
  tkgzef <= tkgzef;
  hnuio <= hnuio;
  
  -- Multi-driven assignments
  ooaxovcsd <= tlvt;
  y <= "H0";
  fzvis <= 'W';
end grgzmpvprc;

entity zeqehl is
  port (pvmrc : in boolean_vector(0 to 4); pnfnepjg : buffer real; qjvrlvo : out time);
end zeqehl;

architecture tjy of zeqehl is
  
begin
  -- Single-driven assignments
  pnfnepjg <= pnfnepjg;
  qjvrlvo <= qjvrlvo;
end tjy;



-- Seed after: 7267335286310566820,6000118208082478503

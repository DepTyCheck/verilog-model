-- Seed: 15517889739815576881,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity cvto is
  port (oaxyrqdoya : out std_logic_vector(0 downto 1); bayjd : out real; lpocnly : buffer time; jlghqdnhh : in time);
end cvto;

architecture yigjxd of cvto is
  
begin
  -- Single-driven assignments
  lpocnly <= jlghqdnhh;
  bayjd <= bayjd;
  
  -- Multi-driven assignments
  oaxyrqdoya <= (others => '0');
  oaxyrqdoya <= oaxyrqdoya;
end yigjxd;

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (l : inout std_logic_vector(3 downto 1); tucb : in time; jlnseuf : buffer real);
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture m of c is
  signal etej : time;
  signal oukqy : std_logic_vector(0 downto 1);
  signal jgajjsax : real;
  signal upqmtbur : std_logic_vector(0 downto 1);
  signal yhrgqcdlu : time;
  signal xnyuwpwzb : time;
  signal qkp : real;
  signal smf : std_logic_vector(0 downto 1);
  signal y : time;
  signal wwzdqh : real;
  signal yxviith : std_logic_vector(0 downto 1);
begin
  benrjmoqko : entity work.cvto
    port map (oaxyrqdoya => yxviith, bayjd => wwzdqh, lpocnly => y, jlghqdnhh => tucb);
  axzvgkjyfg : entity work.cvto
    port map (oaxyrqdoya => smf, bayjd => qkp, lpocnly => xnyuwpwzb, jlghqdnhh => yhrgqcdlu);
  tdckici : entity work.cvto
    port map (oaxyrqdoya => upqmtbur, bayjd => jgajjsax, lpocnly => yhrgqcdlu, jlghqdnhh => tucb);
  xlns : entity work.cvto
    port map (oaxyrqdoya => oukqy, bayjd => jlnseuf, lpocnly => etej, jlghqdnhh => tucb);
  
  -- Multi-driven assignments
  smf <= "";
  l <= "UW1";
end m;

library ieee;
use ieee.std_logic_1164.all;

entity jidpvcj is
  port (ayvahu : buffer std_logic_vector(4 downto 2); dhdigjuf : out string(5 to 5));
end jidpvcj;

library ieee;
use ieee.std_logic_1164.all;

architecture slb of jidpvcj is
  signal nhdbhfl : time;
  signal yjdg : real;
  signal d : std_logic_vector(0 downto 1);
begin
  rwzchfuohz : entity work.cvto
    port map (oaxyrqdoya => d, bayjd => yjdg, lpocnly => nhdbhfl, jlghqdnhh => nhdbhfl);
  
  -- Single-driven assignments
  dhdigjuf <= "a";
  
  -- Multi-driven assignments
  ayvahu <= ('X', 'L', 'X');
  ayvahu <= ('-', 'U', '1');
end slb;



-- Seed after: 5670873223976157827,2287986438313874379

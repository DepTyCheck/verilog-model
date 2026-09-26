-- Seed: 1047119340813114107,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity gjibvqo is
  port (npzszfolqc : buffer time; c : buffer std_logic_vector(4 to 1));
end gjibvqo;

architecture cpc of gjibvqo is
  
begin
  -- Single-driven assignments
  npzszfolqc <= npzszfolqc;
  
  -- Multi-driven assignments
  c <= (others => '0');
end cpc;

entity dbu is
  port (pxdyxdmyl : out string(5 to 2); ep : buffer bit_vector(2 downto 2));
end dbu;

library ieee;
use ieee.std_logic_1164.all;

architecture cmx of dbu is
  signal pe : std_logic_vector(4 to 1);
  signal ysx : time;
  signal srnyoftb : time;
  signal ssdyq : std_logic_vector(4 to 1);
  signal tg : time;
begin
  erasi : entity work.gjibvqo
    port map (npzszfolqc => tg, c => ssdyq);
  hlsctuwn : entity work.gjibvqo
    port map (npzszfolqc => srnyoftb, c => ssdyq);
  a : entity work.gjibvqo
    port map (npzszfolqc => ysx, c => pe);
  
  -- Single-driven assignments
  ep <= (others => '0');
  pxdyxdmyl <= (others => ' ');
  
  -- Multi-driven assignments
  pe <= ssdyq;
  ssdyq <= ssdyq;
  pe <= "";
  ssdyq <= (others => '0');
end cmx;

library ieee;
use ieee.std_logic_1164.all;

entity tspzkz is
  port (cx : linkage std_logic; bea : out severity_level; fxjnwcuaq : in std_logic);
end tspzkz;

architecture el of tspzkz is
  signal kavqh : bit_vector(2 downto 2);
  signal qgx : string(5 to 2);
  signal ydzlvsvo : bit_vector(2 downto 2);
  signal dbnevasi : string(5 to 2);
begin
  oxhl : entity work.dbu
    port map (pxdyxdmyl => dbnevasi, ep => ydzlvsvo);
  tlurgsriun : entity work.dbu
    port map (pxdyxdmyl => qgx, ep => kavqh);
  
  -- Single-driven assignments
  bea <= ERROR;
end el;



-- Seed after: 3136751646342183959,10875537289884587119

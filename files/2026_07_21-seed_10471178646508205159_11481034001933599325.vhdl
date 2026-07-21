-- Seed: 10471178646508205159,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity vaggce is
  port (itrgpihjou : inout std_logic; teu : inout time_vector(2 to 2));
end vaggce;

architecture nygx of vaggce is
  
begin
  -- Multi-driven assignments
  itrgpihjou <= itrgpihjou;
  itrgpihjou <= 'X';
  itrgpihjou <= '-';
  itrgpihjou <= itrgpihjou;
end nygx;

entity pixjuzunr is
  port (ak : buffer time);
end pixjuzunr;

library ieee;
use ieee.std_logic_1164.all;

architecture az of pixjuzunr is
  signal jgi : time_vector(2 to 2);
  signal cz : time_vector(2 to 2);
  signal owpnjrez : std_logic;
  signal r : time_vector(2 to 2);
  signal dq : std_logic;
begin
  bivjiv : entity work.vaggce
    port map (itrgpihjou => dq, teu => r);
  wyxrzyjgk : entity work.vaggce
    port map (itrgpihjou => owpnjrez, teu => cz);
  dpi : entity work.vaggce
    port map (itrgpihjou => owpnjrez, teu => jgi);
  
  -- Single-driven assignments
  ak <= 16#D# ps;
  
  -- Multi-driven assignments
  dq <= 'X';
  dq <= dq;
end az;

library ieee;
use ieee.std_logic_1164.all;

entity ssfwvxssc is
  port (asy : out time; w : buffer std_logic; hoprryb : in std_logic_vector(1 to 3); fehthq : buffer std_logic_vector(4 downto 2));
end ssfwvxssc;

library ieee;
use ieee.std_logic_1164.all;

architecture o of ssfwvxssc is
  signal oavriug : time_vector(2 to 2);
  signal mclty : std_logic;
  signal doqhltzq : time;
  signal bvkgrf : time;
begin
  mqgvko : entity work.pixjuzunr
    port map (ak => bvkgrf);
  ldu : entity work.pixjuzunr
    port map (ak => doqhltzq);
  ej : entity work.vaggce
    port map (itrgpihjou => mclty, teu => oavriug);
  ljq : entity work.pixjuzunr
    port map (ak => asy);
  
  -- Multi-driven assignments
  fehthq <= hoprryb;
  w <= 'X';
  w <= w;
  fehthq <= "111";
end o;



-- Seed after: 8961547978203675792,11481034001933599325

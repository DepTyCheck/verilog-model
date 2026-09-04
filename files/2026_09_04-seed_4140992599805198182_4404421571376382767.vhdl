-- Seed: 4140992599805198182,4404421571376382767

entity yxpqizhe is
  port (ejttzjfuz : inout string(2 downto 2));
end yxpqizhe;

architecture py of yxpqizhe is
  
begin
  -- Single-driven assignments
  ejttzjfuz <= ejttzjfuz;
end py;

library ieee;
use ieee.std_logic_1164.all;

entity raw is
  port (blypsmoh : buffer integer_vector(1 downto 3); ungiykzfc : out std_logic_vector(0 downto 2); rlxykbqqb : out time);
end raw;

architecture depusxoxh of raw is
  signal adsgudoi : string(2 downto 2);
  signal o : string(2 downto 2);
  signal bgrzfcrom : string(2 downto 2);
begin
  rae : entity work.yxpqizhe
    port map (ejttzjfuz => bgrzfcrom);
  ifot : entity work.yxpqizhe
    port map (ejttzjfuz => o);
  nshxj : entity work.yxpqizhe
    port map (ejttzjfuz => adsgudoi);
  
  -- Single-driven assignments
  rlxykbqqb <= rlxykbqqb;
  blypsmoh <= blypsmoh;
  
  -- Multi-driven assignments
  ungiykzfc <= "";
end depusxoxh;

entity ynlhj is
  port (xncmtvdts : out bit_vector(2 downto 4); x : in time_vector(3 to 3));
end ynlhj;

library ieee;
use ieee.std_logic_1164.all;

architecture xmpb of ynlhj is
  signal raryqlfd : time;
  signal jdp : std_logic_vector(0 downto 2);
  signal lcsrcmxwyk : integer_vector(1 downto 3);
begin
  kqlcy : entity work.raw
    port map (blypsmoh => lcsrcmxwyk, ungiykzfc => jdp, rlxykbqqb => raryqlfd);
  
  -- Single-driven assignments
  xncmtvdts <= xncmtvdts;
  
  -- Multi-driven assignments
  jdp <= (others => '0');
  jdp <= (others => '0');
  jdp <= jdp;
  jdp <= (others => '0');
end xmpb;



-- Seed after: 11523354095101975248,4404421571376382767

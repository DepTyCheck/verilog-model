-- Seed: 11492662976142324792,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity horgeb is
  port (jx : out real_vector(4 downto 2); uhqpx : out time; cdzit : in std_logic_vector(1 to 0));
end horgeb;

architecture htdgzxmus of horgeb is
  
begin
  -- Single-driven assignments
  uhqpx <= uhqpx;
  jx <= jx;
end htdgzxmus;

library ieee;
use ieee.std_logic_1164.all;

entity vbjyacu is
  port (sithesxc : linkage std_logic_vector(1 downto 4); jrax : in real; xpbcehekgd : in std_logic; kdhdm : in std_logic_vector(0 downto 1));
end vbjyacu;

library ieee;
use ieee.std_logic_1164.all;

architecture hwulklz of vbjyacu is
  signal avvyjuxc : std_logic_vector(1 to 0);
  signal tjprprtm : time;
  signal sql : real_vector(4 downto 2);
  signal r : std_logic_vector(1 to 0);
  signal h : time;
  signal lk : real_vector(4 downto 2);
  signal xs : time;
  signal ekjvbkflb : real_vector(4 downto 2);
begin
  axzqwq : entity work.horgeb
    port map (jx => ekjvbkflb, uhqpx => xs, cdzit => kdhdm);
  ipctwzv : entity work.horgeb
    port map (jx => lk, uhqpx => h, cdzit => r);
  ue : entity work.horgeb
    port map (jx => sql, uhqpx => tjprprtm, cdzit => avvyjuxc);
  
  -- Multi-driven assignments
  avvyjuxc <= kdhdm;
  r <= (others => '0');
end hwulklz;



-- Seed after: 6659638963930288464,8067602802092121131

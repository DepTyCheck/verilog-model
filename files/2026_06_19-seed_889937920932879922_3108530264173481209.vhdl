-- Seed: 889937920932879922,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity tcpnopnjry is
  port (sgnkb : inout std_logic; es : inout integer_vector(1 to 3));
end tcpnopnjry;

architecture cxxkvscsxq of tcpnopnjry is
  
begin
  -- Single-driven assignments
  es <= (0302, 16#F_5_9_C_F#, 16#A514#);
  
  -- Multi-driven assignments
  sgnkb <= 'X';
  sgnkb <= 'Z';
  sgnkb <= '1';
  sgnkb <= 'L';
end cxxkvscsxq;

library ieee;
use ieee.std_logic_1164.all;

entity refa is
  port (fkyatyp : in std_logic; rdcp : out std_logic_vector(2 downto 4); hr : inout string(4 downto 2));
end refa;

library ieee;
use ieee.std_logic_1164.all;

architecture xzotyyecka of refa is
  signal phuh : integer_vector(1 to 3);
  signal sqpqbdobhj : std_logic;
  signal zimcvkd : integer_vector(1 to 3);
  signal jnvx : integer_vector(1 to 3);
  signal ldyiqcga : integer_vector(1 to 3);
  signal hyoyrxhl : std_logic;
begin
  zun : entity work.tcpnopnjry
    port map (sgnkb => hyoyrxhl, es => ldyiqcga);
  qqegkjax : entity work.tcpnopnjry
    port map (sgnkb => hyoyrxhl, es => jnvx);
  ezegheezw : entity work.tcpnopnjry
    port map (sgnkb => hyoyrxhl, es => zimcvkd);
  nezcnxayl : entity work.tcpnopnjry
    port map (sgnkb => sqpqbdobhj, es => phuh);
  
  -- Single-driven assignments
  hr <= "kzk";
end xzotyyecka;

entity qbp is
  port (ftszw : in character; gdomnpvkmh : buffer real);
end qbp;

library ieee;
use ieee.std_logic_1164.all;

architecture fzba of qbp is
  signal ydkmtw : string(4 downto 2);
  signal iufiwuhn : std_logic_vector(2 downto 4);
  signal vpf : std_logic;
  signal qaap : integer_vector(1 to 3);
  signal y : std_logic;
begin
  rm : entity work.tcpnopnjry
    port map (sgnkb => y, es => qaap);
  zuuxfotqh : entity work.refa
    port map (fkyatyp => vpf, rdcp => iufiwuhn, hr => ydkmtw);
  
  -- Single-driven assignments
  gdomnpvkmh <= 8#35.4041#;
end fzba;



-- Seed after: 7413264737910460033,3108530264173481209

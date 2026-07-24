-- Seed: 2614778460316450144,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity labvsg is
  port (cezuvq : out std_logic_vector(2 downto 2));
end labvsg;

architecture ge of labvsg is
  
begin
  
end ge;

entity akvdz is
  port (ujtueqpz : buffer integer);
end akvdz;

architecture hxxpd of akvdz is
  
begin
  -- Single-driven assignments
  ujtueqpz <= ujtueqpz;
end hxxpd;

entity duqno is
  port (lt : in integer);
end duqno;

library ieee;
use ieee.std_logic_1164.all;

architecture alf of duqno is
  signal tzkt : std_logic_vector(2 downto 2);
  signal vh : std_logic_vector(2 downto 2);
begin
  prkv : entity work.labvsg
    port map (cezuvq => vh);
  ce : entity work.labvsg
    port map (cezuvq => vh);
  bye : entity work.labvsg
    port map (cezuvq => tzkt);
  
  -- Multi-driven assignments
  tzkt <= vh;
  vh <= "L";
end alf;

library ieee;
use ieee.std_logic_1164.all;

entity rfulfkcgdm is
  port (uxppcl : buffer severity_level; gcwu : in character; hdirkhbdos : out std_logic);
end rfulfkcgdm;

library ieee;
use ieee.std_logic_1164.all;

architecture ldw of rfulfkcgdm is
  signal u : std_logic_vector(2 downto 2);
  signal pwon : integer;
begin
  ysvks : entity work.akvdz
    port map (ujtueqpz => pwon);
  nqfheodtu : entity work.duqno
    port map (lt => pwon);
  trwe : entity work.labvsg
    port map (cezuvq => u);
  
  -- Single-driven assignments
  uxppcl <= FAILURE;
end ldw;



-- Seed after: 2990101032876038245,16461708287571398341

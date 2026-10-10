-- Seed: 10836954067811548304,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity sjr is
  port (kohgu : in time; g : inout std_logic_vector(3 downto 1); fhkhnqh : inout time_vector(0 downto 1); ahkbkq : in real);
end sjr;

architecture xejd of sjr is
  
begin
  -- Multi-driven assignments
  g <= ('H', 'U', 'H');
  g <= ('W', '1', 'X');
  g <= ('H', '-', 'H');
end xejd;

entity blojtcmh is
  port (hlvipyedm : linkage integer; fbulzqznj : out integer);
end blojtcmh;

library ieee;
use ieee.std_logic_1164.all;

architecture rgcqvacehs of blojtcmh is
  signal njpbltemz : real;
  signal wsgcwko : time_vector(0 downto 1);
  signal kjgdumb : time;
  signal eojhedb : real;
  signal ytdqmam : time_vector(0 downto 1);
  signal wdkmttdw : std_logic_vector(3 downto 1);
  signal b : time;
  signal lksk : real;
  signal ytzci : time_vector(0 downto 1);
  signal pcf : std_logic_vector(3 downto 1);
  signal sjod : time;
begin
  ckdibjyjvg : entity work.sjr
    port map (kohgu => sjod, g => pcf, fhkhnqh => ytzci, ahkbkq => lksk);
  untb : entity work.sjr
    port map (kohgu => b, g => wdkmttdw, fhkhnqh => ytdqmam, ahkbkq => eojhedb);
  mbdodwz : entity work.sjr
    port map (kohgu => kjgdumb, g => pcf, fhkhnqh => wsgcwko, ahkbkq => njpbltemz);
  
  -- Multi-driven assignments
  pcf <= "L-L";
end rgcqvacehs;

library ieee;
use ieee.std_logic_1164.all;

entity xkvgvyqj is
  port (jw : buffer time; trjydkneyh : out std_logic; lgr : linkage boolean);
end xkvgvyqj;

architecture aksst of xkvgvyqj is
  
begin
  -- Single-driven assignments
  jw <= jw;
  
  -- Multi-driven assignments
  trjydkneyh <= 'H';
end aksst;

library ieee;
use ieee.std_logic_1164.all;

entity jlx is
  port (cptrxhy : in real; ttixqqlkg : buffer boolean; wuhhh : out std_logic; ntjugun : out integer);
end jlx;

library ieee;
use ieee.std_logic_1164.all;

architecture tijyown of jlx is
  signal vcsb : boolean;
  signal ojuqlbxja : std_logic;
  signal pbyktrfhy : integer;
  signal fo : integer;
  signal x : time_vector(0 downto 1);
  signal roqysqu : std_logic_vector(3 downto 1);
  signal vybgzvxyhp : time;
begin
  ljacfgk : entity work.sjr
    port map (kohgu => vybgzvxyhp, g => roqysqu, fhkhnqh => x, ahkbkq => cptrxhy);
  k : entity work.blojtcmh
    port map (hlvipyedm => fo, fbulzqznj => pbyktrfhy);
  fhropyms : entity work.xkvgvyqj
    port map (jw => vybgzvxyhp, trjydkneyh => ojuqlbxja, lgr => vcsb);
end tijyown;



-- Seed after: 580147243915523585,511364357853360275

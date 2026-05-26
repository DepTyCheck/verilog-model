-- Seed: 16028138036121803657,8089241273282434469

library ieee;
use ieee.std_logic_1164.all;

entity wg is
  port (tvorkyct : out std_logic; fz : out std_logic_vector(3 to 3); pg : out time_vector(4 to 3));
end wg;



architecture meiuj of wg is
  
begin
  
end meiuj;



entity qqsp is
  port (mawod : in bit);
end qqsp;

library ieee;
use ieee.std_logic_1164.all;

architecture uaoc of qqsp is
  signal vuhowmewf : time_vector(4 to 3);
  signal k : time_vector(4 to 3);
  signal ixxsn : std_logic;
  signal ul : time_vector(4 to 3);
  signal fu : std_logic;
  signal atjeaovpid : time_vector(4 to 3);
  signal y : std_logic_vector(3 to 3);
  signal d : std_logic;
begin
  atdctg : entity work.wg
    port map (tvorkyct => d, fz => y, pg => atjeaovpid);
  kvenpgqoe : entity work.wg
    port map (tvorkyct => fu, fz => y, pg => ul);
  voerhxi : entity work.wg
    port map (tvorkyct => ixxsn, fz => y, pg => k);
  acb : entity work.wg
    port map (tvorkyct => d, fz => y, pg => vuhowmewf);
end uaoc;

library ieee;
use ieee.std_logic_1164.all;

entity xsgm is
  port (yjmjniug : inout std_logic_vector(2 to 4); izqj : in real; nq : out time_vector(2 downto 3));
end xsgm;

library ieee;
use ieee.std_logic_1164.all;

architecture dalewxrkug of xsgm is
  signal se : time_vector(4 to 3);
  signal jh : std_logic_vector(3 to 3);
  signal kwlaxyutv : std_logic;
begin
  be : entity work.wg
    port map (tvorkyct => kwlaxyutv, fz => jh, pg => se);
end dalewxrkug;



entity ajwzdny is
  port (sihshkdnb : inout real; awjl : inout bit; weo : linkage severity_level);
end ajwzdny;

library ieee;
use ieee.std_logic_1164.all;

architecture giehwn of ajwzdny is
  signal wzemmnhvjt : time_vector(4 to 3);
  signal wueqfs : std_logic_vector(3 to 3);
  signal rqcspubrdx : std_logic;
  signal hred : bit;
begin
  prdv : entity work.qqsp
    port map (mawod => hred);
  xb : entity work.wg
    port map (tvorkyct => rqcspubrdx, fz => wueqfs, pg => wzemmnhvjt);
end giehwn;



-- Seed after: 17712845973938176580,8089241273282434469

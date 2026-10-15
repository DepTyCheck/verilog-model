-- Seed: 17041040403959312758,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity vem is
  port (xl : out std_logic; ezabl : buffer string(3 downto 2); fjom : buffer time);
end vem;

architecture qan of vem is
  
begin
  -- Multi-driven assignments
  xl <= 'X';
end qan;

entity w is
  port (glzemxtkk : linkage bit_vector(2 downto 1); eddqof : inout real);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture wianrmclql of w is
  signal qgaevhftmd : time;
  signal ediwhksw : string(3 downto 2);
  signal amipamweki : time;
  signal pxxa : string(3 downto 2);
  signal i : std_logic;
begin
  jj : entity work.vem
    port map (xl => i, ezabl => pxxa, fjom => amipamweki);
  ca : entity work.vem
    port map (xl => i, ezabl => ediwhksw, fjom => qgaevhftmd);
  
  -- Multi-driven assignments
  i <= i;
end wianrmclql;

entity vnuwbnovjy is
  port (ypjumsn : linkage integer; hjjxfdpmy : linkage time; ym : in time_vector(3 to 1));
end vnuwbnovjy;

library ieee;
use ieee.std_logic_1164.all;

architecture ca of vnuwbnovjy is
  signal lnwlspk : real;
  signal hndcjvxsz : bit_vector(2 downto 1);
  signal q : time;
  signal krrwsmfgfa : string(3 downto 2);
  signal aincnvk : time;
  signal olcr : string(3 downto 2);
  signal vnxl : std_logic;
begin
  hxlpiah : entity work.vem
    port map (xl => vnxl, ezabl => olcr, fjom => aincnvk);
  vwndhnhzdv : entity work.vem
    port map (xl => vnxl, ezabl => krrwsmfgfa, fjom => q);
  onstads : entity work.w
    port map (glzemxtkk => hndcjvxsz, eddqof => lnwlspk);
  
  -- Multi-driven assignments
  vnxl <= 'L';
  vnxl <= 'H';
  vnxl <= vnxl;
end ca;

entity ifgu is
  port (ypqhcard : linkage integer; ego : out time; jhj : linkage time);
end ifgu;

library ieee;
use ieee.std_logic_1164.all;

architecture rloyncmsn of ifgu is
  signal yfyqqh : time_vector(3 to 1);
  signal mzkflzwa : integer;
  signal dvmx : time;
  signal wlqfm : string(3 downto 2);
  signal yeqlqwct : std_logic;
  signal zn : time_vector(3 to 1);
  signal xqevhj : time;
  signal wx : integer;
begin
  cahzattvoj : entity work.vnuwbnovjy
    port map (ypjumsn => wx, hjjxfdpmy => xqevhj, ym => zn);
  d : entity work.vem
    port map (xl => yeqlqwct, ezabl => wlqfm, fjom => dvmx);
  fc : entity work.vnuwbnovjy
    port map (ypjumsn => mzkflzwa, hjjxfdpmy => jhj, ym => zn);
  brsnb : entity work.vnuwbnovjy
    port map (ypjumsn => ypqhcard, hjjxfdpmy => ego, ym => yfyqqh);
  
  -- Single-driven assignments
  zn <= (others => 0 ns);
  
  -- Multi-driven assignments
  yeqlqwct <= '-';
  yeqlqwct <= yeqlqwct;
end rloyncmsn;



-- Seed after: 11712450296038498273,6377581586043967497

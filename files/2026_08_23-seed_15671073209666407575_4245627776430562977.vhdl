-- Seed: 15671073209666407575,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity tdvmum is
  port (qaqqsw : out time; qrks : out std_logic; mrtrngdcs : buffer real);
end tdvmum;

architecture z of tdvmum is
  
begin
  -- Single-driven assignments
  qaqqsw <= 8#1# us;
  mrtrngdcs <= 4.0_1_4;
  
  -- Multi-driven assignments
  qrks <= qrks;
end z;

library ieee;
use ieee.std_logic_1164.all;

entity dg is
  port (yjc : inout std_logic; ags : out severity_level; ppipket : in integer; ixwkz : inout std_logic);
end dg;

library ieee;
use ieee.std_logic_1164.all;

architecture ffss of dg is
  signal guahtkojz : real;
  signal rkyvuhx : std_logic;
  signal vsx : time;
  signal ochnshtae : real;
  signal w : std_logic;
  signal vmgky : time;
  signal rtyhbdbdzr : real;
  signal dderjhgry : std_logic;
  signal fnnkl : time;
begin
  pgp : entity work.tdvmum
    port map (qaqqsw => fnnkl, qrks => dderjhgry, mrtrngdcs => rtyhbdbdzr);
  yug : entity work.tdvmum
    port map (qaqqsw => vmgky, qrks => w, mrtrngdcs => ochnshtae);
  a : entity work.tdvmum
    port map (qaqqsw => vsx, qrks => rkyvuhx, mrtrngdcs => guahtkojz);
  
  -- Single-driven assignments
  ags <= ERROR;
  
  -- Multi-driven assignments
  w <= ixwkz;
  w <= 'Z';
  w <= ixwkz;
  w <= 'Z';
end ffss;

library ieee;
use ieee.std_logic_1164.all;

entity wznex is
  port (fzcg : buffer std_logic_vector(3 to 1); vzxdrnog : buffer std_logic_vector(2 to 2); k : buffer boolean_vector(2 to 1));
end wznex;

library ieee;
use ieee.std_logic_1164.all;

architecture xdhmfwjyq of wznex is
  signal eviaxzake : real;
  signal qeoksvl : time;
  signal h : integer;
  signal iflfchnca : severity_level;
  signal drzcerhuv : std_logic;
  signal fiuppnz : real;
  signal wxnt : std_logic;
  signal bwphdnkaxz : time;
  signal ppowbzzjw : real;
  signal mzhhqq : std_logic;
  signal ih : time;
begin
  ss : entity work.tdvmum
    port map (qaqqsw => ih, qrks => mzhhqq, mrtrngdcs => ppowbzzjw);
  efagj : entity work.tdvmum
    port map (qaqqsw => bwphdnkaxz, qrks => wxnt, mrtrngdcs => fiuppnz);
  azpzbnqvuj : entity work.dg
    port map (yjc => drzcerhuv, ags => iflfchnca, ppipket => h, ixwkz => wxnt);
  msdaac : entity work.tdvmum
    port map (qaqqsw => qeoksvl, qrks => mzhhqq, mrtrngdcs => eviaxzake);
  
  -- Single-driven assignments
  k <= k;
  
  -- Multi-driven assignments
  vzxdrnog <= vzxdrnog;
  vzxdrnog <= vzxdrnog;
  fzcg <= fzcg;
  vzxdrnog <= vzxdrnog;
end xdhmfwjyq;



-- Seed after: 14609872264738554361,4245627776430562977

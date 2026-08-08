-- Seed: 273523842504157706,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity bgnstzcb is
  port (di : linkage std_logic; ykkhlpfk : out character; ocm : buffer time; kck : inout integer);
end bgnstzcb;

architecture flm of bgnstzcb is
  
begin
  -- Single-driven assignments
  kck <= 2#1_1_1_0_0#;
  ykkhlpfk <= 'l';
end flm;

entity ygzx is
  port (ljbhfi : inout severity_level; zxgisrx : inout boolean);
end ygzx;

library ieee;
use ieee.std_logic_1164.all;

architecture fomj of ygzx is
  signal tqvpfbj : integer;
  signal p : time;
  signal hwsdiqs : character;
  signal r : std_logic;
  signal ntzbelp : integer;
  signal fjeef : time;
  signal pjfnfrmcgq : character;
  signal onpu : integer;
  signal npjlz : time;
  signal msrhmgtkn : character;
  signal z : integer;
  signal zebwdah : time;
  signal uc : character;
  signal sy : std_logic;
begin
  gki : entity work.bgnstzcb
    port map (di => sy, ykkhlpfk => uc, ocm => zebwdah, kck => z);
  smlyhoduvq : entity work.bgnstzcb
    port map (di => sy, ykkhlpfk => msrhmgtkn, ocm => npjlz, kck => onpu);
  rbrnvzi : entity work.bgnstzcb
    port map (di => sy, ykkhlpfk => pjfnfrmcgq, ocm => fjeef, kck => ntzbelp);
  qspncmrbl : entity work.bgnstzcb
    port map (di => r, ykkhlpfk => hwsdiqs, ocm => p, kck => tqvpfbj);
  
  -- Single-driven assignments
  zxgisrx <= zxgisrx;
  ljbhfi <= FAILURE;
  
  -- Multi-driven assignments
  r <= sy;
end fomj;

entity w is
  port (cwpjub : inout time);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture fz of w is
  signal zweulpruhb : integer;
  signal bs : character;
  signal jito : std_logic;
  signal rl : integer;
  signal iqsus : time;
  signal ufpoyudrgc : character;
  signal abodu : std_logic;
  signal k : boolean;
  signal ja : severity_level;
begin
  jkucnekw : entity work.ygzx
    port map (ljbhfi => ja, zxgisrx => k);
  atkcmrzxr : entity work.bgnstzcb
    port map (di => abodu, ykkhlpfk => ufpoyudrgc, ocm => iqsus, kck => rl);
  ib : entity work.bgnstzcb
    port map (di => jito, ykkhlpfk => bs, ocm => cwpjub, kck => zweulpruhb);
  
  -- Multi-driven assignments
  abodu <= abodu;
  jito <= abodu;
  jito <= '1';
  abodu <= 'L';
end fz;



-- Seed after: 10572086291333893977,8927267689619684183

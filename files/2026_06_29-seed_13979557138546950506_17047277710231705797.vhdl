-- Seed: 13979557138546950506,17047277710231705797

entity klkusm is
  port (wnpuyudvo : buffer real; svryw : inout integer; iemmphqla : out integer);
end klkusm;

architecture ov of klkusm is
  
begin
  -- Single-driven assignments
  iemmphqla <= 0;
  wnpuyudvo <= 16#B_F_0_8_4.0#;
  svryw <= 2#00111#;
end ov;

library ieee;
use ieee.std_logic_1164.all;

entity ckfxjqhiu is
  port (sfntukrcmo : buffer std_logic_vector(0 downto 3); eepajsb : out std_logic; bzveesfwbu : in integer);
end ckfxjqhiu;

architecture qxkjzaqnr of ckfxjqhiu is
  signal waobtyrrrg : integer;
  signal h : integer;
  signal cn : real;
  signal gtn : integer;
  signal vvetb : integer;
  signal eifocq : real;
  signal fnqkbmhh : integer;
  signal ptyz : integer;
  signal ugtvapwvox : real;
  signal vbrxe : integer;
  signal erufofbdv : integer;
  signal unuwstgrm : real;
begin
  aubawyrmoe : entity work.klkusm
    port map (wnpuyudvo => unuwstgrm, svryw => erufofbdv, iemmphqla => vbrxe);
  vcjrs : entity work.klkusm
    port map (wnpuyudvo => ugtvapwvox, svryw => ptyz, iemmphqla => fnqkbmhh);
  ktqwrywr : entity work.klkusm
    port map (wnpuyudvo => eifocq, svryw => vvetb, iemmphqla => gtn);
  tqbd : entity work.klkusm
    port map (wnpuyudvo => cn, svryw => h, iemmphqla => waobtyrrrg);
  
  -- Multi-driven assignments
  eepajsb <= 'U';
  eepajsb <= 'W';
end qxkjzaqnr;

library ieee;
use ieee.std_logic_1164.all;

entity jiqpssr is
  port (lws : inout time; ae : buffer bit_vector(4 to 3); oopksnh : out std_logic_vector(1 to 0));
end jiqpssr;

library ieee;
use ieee.std_logic_1164.all;

architecture kb of jiqpssr is
  signal biulrn : std_logic;
  signal ewfvp : std_logic_vector(0 downto 3);
  signal jyuurw : integer;
  signal pve : real;
  signal uxs : integer;
  signal hksbz : std_logic;
begin
  fxrnhmadi : entity work.ckfxjqhiu
    port map (sfntukrcmo => oopksnh, eepajsb => hksbz, bzveesfwbu => uxs);
  jwgyn : entity work.klkusm
    port map (wnpuyudvo => pve, svryw => uxs, iemmphqla => jyuurw);
  hoqxtjl : entity work.ckfxjqhiu
    port map (sfntukrcmo => ewfvp, eepajsb => biulrn, bzveesfwbu => jyuurw);
  
  -- Single-driven assignments
  lws <= 0.23 ms;
  ae <= (others => '0');
  
  -- Multi-driven assignments
  ewfvp <= (others => '0');
  oopksnh <= (others => '0');
  hksbz <= 'Z';
  biulrn <= 'L';
end kb;

library ieee;
use ieee.std_logic_1164.all;

entity sofbddbu is
  port (j : in bit; ceslcwp : linkage std_logic_vector(1 downto 4); giifdhvner : inout real_vector(4 to 4));
end sofbddbu;

library ieee;
use ieee.std_logic_1164.all;

architecture m of sofbddbu is
  signal bqnfxc : std_logic_vector(1 to 0);
  signal igv : bit_vector(4 to 3);
  signal ioejuly : time;
  signal arvtl : integer;
  signal spljq : std_logic;
  signal mtmf : std_logic_vector(0 downto 3);
begin
  wn : entity work.ckfxjqhiu
    port map (sfntukrcmo => mtmf, eepajsb => spljq, bzveesfwbu => arvtl);
  kgttfgzab : entity work.jiqpssr
    port map (lws => ioejuly, ae => igv, oopksnh => bqnfxc);
  
  -- Single-driven assignments
  giifdhvner <= (others => 16#07D.5_3_4_8_7#);
  arvtl <= 0;
  
  -- Multi-driven assignments
  spljq <= 'Z';
end m;



-- Seed after: 11533247439209406692,17047277710231705797

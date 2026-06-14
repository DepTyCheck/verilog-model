-- Seed: 12270678212375558498,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity jwkztc is
  port (f : inout time; xerpiqumh : out time; nhkuxqo : buffer bit; l : in std_logic);
end jwkztc;

architecture omcfecnrk of jwkztc is
  
begin
  -- Single-driven assignments
  f <= 16#BD5A# ns;
  xerpiqumh <= 3_2_3 ms;
  nhkuxqo <= '1';
end omcfecnrk;

library ieee;
use ieee.std_logic_1164.all;

entity hdjd is
  port (yd : linkage std_logic; fftmf : inout std_logic_vector(1 downto 0); cutijjt : inout integer; vdfirsv : out std_logic_vector(2 downto 1));
end hdjd;

library ieee;
use ieee.std_logic_1164.all;

architecture zzhhb of hdjd is
  signal wp : std_logic;
  signal cmzsljgtz : bit;
  signal vyynkij : time;
  signal zmizitz : time;
begin
  jzpfgwshr : entity work.jwkztc
    port map (f => zmizitz, xerpiqumh => vyynkij, nhkuxqo => cmzsljgtz, l => wp);
  
  -- Single-driven assignments
  cutijjt <= 2;
  
  -- Multi-driven assignments
  vdfirsv <= "0Z";
end zzhhb;

entity kiyxwopb is
  port (bs : buffer severity_level);
end kiyxwopb;

library ieee;
use ieee.std_logic_1164.all;

architecture dfujj of kiyxwopb is
  signal jsadae : std_logic_vector(2 downto 1);
  signal hlobpdw : integer;
  signal nnzcigrbw : std_logic_vector(1 downto 0);
  signal kqwwhllrr : std_logic;
  signal v : bit;
  signal qf : time;
  signal d : time;
  signal mrqdrpgc : std_logic;
  signal kaofrro : bit;
  signal wtez : time;
  signal zlruxkwz : time;
begin
  osfgrao : entity work.jwkztc
    port map (f => zlruxkwz, xerpiqumh => wtez, nhkuxqo => kaofrro, l => mrqdrpgc);
  rjbhbvx : entity work.jwkztc
    port map (f => d, xerpiqumh => qf, nhkuxqo => v, l => kqwwhllrr);
  yx : entity work.hdjd
    port map (yd => kqwwhllrr, fftmf => nnzcigrbw, cutijjt => hlobpdw, vdfirsv => jsadae);
  
  -- Single-driven assignments
  bs <= WARNING;
end dfujj;

library ieee;
use ieee.std_logic_1164.all;

entity purwuv is
  port (vofz : in boolean; vskioq : inout std_logic_vector(4 downto 2); y : in integer);
end purwuv;

library ieee;
use ieee.std_logic_1164.all;

architecture fqj of purwuv is
  signal aksgyd : std_logic_vector(2 downto 1);
  signal gbcldv : integer;
  signal ttdsgzellx : std_logic_vector(1 downto 0);
  signal xamn : std_logic;
  signal tkifmai : std_logic;
  signal f : bit;
  signal va : time;
  signal sopgqya : time;
  signal nmvmb : severity_level;
  signal gkk : std_logic;
  signal llayxc : bit;
  signal lla : time;
  signal wcqvjjjia : time;
begin
  lweas : entity work.jwkztc
    port map (f => wcqvjjjia, xerpiqumh => lla, nhkuxqo => llayxc, l => gkk);
  zy : entity work.kiyxwopb
    port map (bs => nmvmb);
  lcqvbjduw : entity work.jwkztc
    port map (f => sopgqya, xerpiqumh => va, nhkuxqo => f, l => tkifmai);
  nryisyc : entity work.hdjd
    port map (yd => xamn, fftmf => ttdsgzellx, cutijjt => gbcldv, vdfirsv => aksgyd);
  
  -- Multi-driven assignments
  vskioq <= ('U', 'W', '0');
  xamn <= '0';
  gkk <= 'U';
  vskioq <= ('U', 'U', 'L');
end fqj;



-- Seed after: 11706765201794142946,14652815260262078753

-- Seed: 12241400491900793515,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity jmk is
  port (jpufgy : linkage time; hctzbxq : buffer real; uswda : in integer; jeghqmyyql : in std_logic);
end jmk;

architecture lmxrslh of jmk is
  
begin
  -- Single-driven assignments
  hctzbxq <= 16#0_4.A_A_3_F_A#;
end lmxrslh;

library ieee;
use ieee.std_logic_1164.all;

entity ibfwg is
  port (w : buffer std_logic; i : inout std_logic_vector(3 downto 0); gvxxezh : linkage real; clkr : buffer integer);
end ibfwg;

library ieee;
use ieee.std_logic_1164.all;

architecture utnr of ibfwg is
  signal yfc : integer;
  signal dubllwj : real;
  signal rit : time;
  signal wvoe : std_logic;
  signal pjvzjqtu : integer;
  signal x : real;
  signal nocojffli : time;
  signal qeq : integer;
  signal skgck : real;
  signal fbodeiugcd : time;
begin
  z : entity work.jmk
    port map (jpufgy => fbodeiugcd, hctzbxq => skgck, uswda => qeq, jeghqmyyql => w);
  zzpse : entity work.jmk
    port map (jpufgy => nocojffli, hctzbxq => x, uswda => pjvzjqtu, jeghqmyyql => wvoe);
  oqcjzntnyt : entity work.jmk
    port map (jpufgy => rit, hctzbxq => dubllwj, uswda => yfc, jeghqmyyql => w);
  
  -- Single-driven assignments
  pjvzjqtu <= clkr;
  clkr <= clkr;
end utnr;

library ieee;
use ieee.std_logic_1164.all;

entity zseiahsi is
  port (ybdvt : linkage character; poxemz : inout std_logic_vector(4 to 3));
end zseiahsi;

library ieee;
use ieee.std_logic_1164.all;

architecture moohsjz of zseiahsi is
  signal bipjfenecp : real;
  signal lkpbrgw : time;
  signal avyda : std_logic;
  signal krs : integer;
  signal xqua : real;
  signal dapd : time;
  signal mhrliopf : real;
  signal hjqzx : time;
  signal clpcy : std_logic;
  signal veuip : integer;
  signal jpkc : real;
  signal wqyhjai : time;
begin
  scrojqsb : entity work.jmk
    port map (jpufgy => wqyhjai, hctzbxq => jpkc, uswda => veuip, jeghqmyyql => clpcy);
  wirz : entity work.jmk
    port map (jpufgy => hjqzx, hctzbxq => mhrliopf, uswda => veuip, jeghqmyyql => clpcy);
  kmbjhyhe : entity work.jmk
    port map (jpufgy => dapd, hctzbxq => xqua, uswda => krs, jeghqmyyql => avyda);
  g : entity work.jmk
    port map (jpufgy => lkpbrgw, hctzbxq => bipjfenecp, uswda => veuip, jeghqmyyql => clpcy);
  
  -- Multi-driven assignments
  avyda <= clpcy;
  poxemz <= (others => '0');
  clpcy <= 'L';
end moohsjz;



-- Seed after: 8933306573896349269,7311216359267151659

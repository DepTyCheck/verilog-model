-- Seed: 16734443101251838365,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (yfxshsaag : buffer std_logic_vector(0 downto 4); ql : linkage std_logic);
end c;

architecture fq of c is
  
begin
  -- Multi-driven assignments
  yfxshsaag <= "";
  yfxshsaag <= yfxshsaag;
  yfxshsaag <= yfxshsaag;
  yfxshsaag <= yfxshsaag;
end fq;

entity gevw is
  port (nhjrdqrdf : linkage string(3 to 1); opmmvycsx : buffer time; zieefevmac : out real);
end gevw;

library ieee;
use ieee.std_logic_1164.all;

architecture dzhuungua of gevw is
  signal tah : std_logic;
  signal rdhj : std_logic_vector(0 downto 4);
  signal t : std_logic;
  signal jzzna : std_logic_vector(0 downto 4);
begin
  wzysizw : entity work.c
    port map (yfxshsaag => jzzna, ql => t);
  fkdkp : entity work.c
    port map (yfxshsaag => jzzna, ql => t);
  zqwqmsopqs : entity work.c
    port map (yfxshsaag => rdhj, ql => tah);
  
  -- Single-driven assignments
  zieefevmac <= 8#3_7_2_7_3.5406#;
end dzhuungua;

library ieee;
use ieee.std_logic_1164.all;

entity lqkgcmnl is
  port (xuo : out real; ufhkv : linkage std_logic);
end lqkgcmnl;

library ieee;
use ieee.std_logic_1164.all;

architecture jlhb of lqkgcmnl is
  signal ircx : std_logic_vector(0 downto 4);
  signal yo : real;
  signal ajnchgo : time;
  signal dpugbirh : string(3 to 1);
begin
  dhr : entity work.gevw
    port map (nhjrdqrdf => dpugbirh, opmmvycsx => ajnchgo, zieefevmac => yo);
  qstpssywi : entity work.c
    port map (yfxshsaag => ircx, ql => ufhkv);
  
  -- Single-driven assignments
  xuo <= yo;
end jlhb;

library ieee;
use ieee.std_logic_1164.all;

entity rpoz is
  port (lfmk : in time; rd : buffer std_logic_vector(2 downto 3));
end rpoz;

library ieee;
use ieee.std_logic_1164.all;

architecture ozflhk of rpoz is
  signal fopxos : real;
  signal hnh : time;
  signal tbmi : string(3 to 1);
  signal ivjq : real;
  signal yvkhgy : time;
  signal mhdwuhdus : string(3 to 1);
  signal qqolqtnmp : std_logic;
  signal mm : std_logic_vector(0 downto 4);
begin
  qndvqdg : entity work.c
    port map (yfxshsaag => mm, ql => qqolqtnmp);
  wfdfqye : entity work.gevw
    port map (nhjrdqrdf => mhdwuhdus, opmmvycsx => yvkhgy, zieefevmac => ivjq);
  ttc : entity work.c
    port map (yfxshsaag => mm, ql => qqolqtnmp);
  dnc : entity work.gevw
    port map (nhjrdqrdf => tbmi, opmmvycsx => hnh, zieefevmac => fopxos);
  
  -- Multi-driven assignments
  rd <= rd;
  rd <= (others => '0');
  rd <= (others => '0');
  rd <= rd;
end ozflhk;



-- Seed after: 3662980332226843018,12536735141187299017

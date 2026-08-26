-- Seed: 1434965945691350902,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity ki is
  port (awoqpkyk : out integer; owigmhbtyw : inout std_logic; bpw : out std_logic_vector(1 downto 4); hehnkxbq : buffer std_logic_vector(0 to 4));
end ki;

architecture vsyoxnawuo of ki is
  
begin
  -- Multi-driven assignments
  bpw <= (others => '0');
  hehnkxbq <= hehnkxbq;
  hehnkxbq <= "ZWX0U";
  bpw <= "";
end vsyoxnawuo;

library ieee;
use ieee.std_logic_1164.all;

entity muvlck is
  port (wuvshrz : out std_logic_vector(1 downto 2); ca : in integer; epn : buffer integer; eg : inout boolean_vector(4 downto 4));
end muvlck;

library ieee;
use ieee.std_logic_1164.all;

architecture wfr of muvlck is
  signal fghsyd : std_logic;
  signal urfgfqjcob : std_logic_vector(0 to 4);
  signal sdcjcy : std_logic_vector(1 downto 4);
  signal ihl : integer;
  signal ed : std_logic_vector(0 to 4);
  signal tcz : std_logic;
  signal k : integer;
  signal mfbhkwuzp : std_logic_vector(0 to 4);
  signal hzwud : std_logic_vector(1 downto 4);
  signal krbuueemo : std_logic;
  signal tfurr : integer;
begin
  usycztjcg : entity work.ki
    port map (awoqpkyk => tfurr, owigmhbtyw => krbuueemo, bpw => hzwud, hehnkxbq => mfbhkwuzp);
  rwe : entity work.ki
    port map (awoqpkyk => k, owigmhbtyw => tcz, bpw => hzwud, hehnkxbq => ed);
  thrrzdkayk : entity work.ki
    port map (awoqpkyk => ihl, owigmhbtyw => krbuueemo, bpw => sdcjcy, hehnkxbq => urfgfqjcob);
  vitlhfxfkf : entity work.ki
    port map (awoqpkyk => epn, owigmhbtyw => fghsyd, bpw => sdcjcy, hehnkxbq => ed);
  
  -- Single-driven assignments
  eg <= (others => FALSE);
  
  -- Multi-driven assignments
  ed <= "ZW0U0";
  wuvshrz <= wuvshrz;
end wfr;

entity oznwsu is
  port (rxrhsw : out integer_vector(2 downto 4); f : out integer);
end oznwsu;

library ieee;
use ieee.std_logic_1164.all;

architecture li of oznwsu is
  signal vetvdypxzn : boolean_vector(4 downto 4);
  signal fuvciymo : integer;
  signal sdi : std_logic_vector(1 downto 2);
  signal azv : std_logic;
  signal aoppt : integer;
  signal gskekfb : std_logic_vector(0 to 4);
  signal trwqxqxi : std_logic_vector(1 downto 4);
  signal ivkuia : std_logic;
  signal unkdbeusd : integer;
begin
  n : entity work.ki
    port map (awoqpkyk => unkdbeusd, owigmhbtyw => ivkuia, bpw => trwqxqxi, hehnkxbq => gskekfb);
  hn : entity work.ki
    port map (awoqpkyk => aoppt, owigmhbtyw => azv, bpw => trwqxqxi, hehnkxbq => gskekfb);
  kbc : entity work.muvlck
    port map (wuvshrz => sdi, ca => fuvciymo, epn => f, eg => vetvdypxzn);
  
  -- Single-driven assignments
  rxrhsw <= rxrhsw;
  
  -- Multi-driven assignments
  ivkuia <= azv;
  ivkuia <= '-';
  gskekfb <= gskekfb;
end li;



-- Seed after: 8130932686978856885,6000118208082478503

-- Seed: 7888557339254010281,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity rn is
  port (lhwf : buffer bit_vector(1 downto 0); mt : in std_logic; octkgoi : in integer; yzkxrivjcg : in std_logic_vector(3 to 2));
end rn;

architecture qha of rn is
  
begin
  
end qha;

entity wczps is
  port (vowq : linkage boolean_vector(0 downto 1); fcfub : buffer real_vector(2 downto 4); zqj : in real);
end wczps;

library ieee;
use ieee.std_logic_1164.all;

architecture kxqlnaamp of wczps is
  signal fwasyaw : std_logic_vector(3 to 2);
  signal ypxpmjvvjs : integer;
  signal uc : std_logic;
  signal ikbkom : bit_vector(1 downto 0);
begin
  n : entity work.rn
    port map (lhwf => ikbkom, mt => uc, octkgoi => ypxpmjvvjs, yzkxrivjcg => fwasyaw);
  
  -- Multi-driven assignments
  uc <= uc;
  uc <= '0';
  uc <= uc;
  uc <= uc;
end kxqlnaamp;

entity sewjqjqou is
  port (gadwhzn : in character; hvlkxv : in integer);
end sewjqjqou;

library ieee;
use ieee.std_logic_1164.all;

architecture bpjfaso of sewjqjqou is
  signal wzafbct : real_vector(2 downto 4);
  signal eumlxtq : boolean_vector(0 downto 1);
  signal njsox : std_logic_vector(3 to 2);
  signal p : bit_vector(1 downto 0);
  signal xmpvjex : std_logic_vector(3 to 2);
  signal zxdddbxwq : std_logic;
  signal li : bit_vector(1 downto 0);
  signal vyekzuts : real;
  signal vvkb : real_vector(2 downto 4);
  signal ecunychxk : boolean_vector(0 downto 1);
begin
  rjcpdql : entity work.wczps
    port map (vowq => ecunychxk, fcfub => vvkb, zqj => vyekzuts);
  m : entity work.rn
    port map (lhwf => li, mt => zxdddbxwq, octkgoi => hvlkxv, yzkxrivjcg => xmpvjex);
  qykv : entity work.rn
    port map (lhwf => p, mt => zxdddbxwq, octkgoi => hvlkxv, yzkxrivjcg => njsox);
  vcbamicg : entity work.wczps
    port map (vowq => eumlxtq, fcfub => wzafbct, zqj => vyekzuts);
  
  -- Single-driven assignments
  vyekzuts <= 0104.1;
  
  -- Multi-driven assignments
  zxdddbxwq <= zxdddbxwq;
end bpjfaso;

entity mldxnfxdz is
  port (ntgyva : linkage integer; tvdjel : buffer time_vector(2 to 4); zam : buffer time; tpa : in boolean);
end mldxnfxdz;

architecture r of mldxnfxdz is
  signal becov : real_vector(2 downto 4);
  signal xuolpevv : boolean_vector(0 downto 1);
  signal uv : real;
  signal aku : real_vector(2 downto 4);
  signal fum : boolean_vector(0 downto 1);
begin
  bgom : entity work.wczps
    port map (vowq => fum, fcfub => aku, zqj => uv);
  ziwzzy : entity work.wczps
    port map (vowq => xuolpevv, fcfub => becov, zqj => uv);
  
  -- Single-driven assignments
  tvdjel <= tvdjel;
  uv <= 2#010.0_0_0_0#;
  zam <= 04.4_3_0_0_3 ms;
end r;



-- Seed after: 15484889765407636937,6562834012285671529

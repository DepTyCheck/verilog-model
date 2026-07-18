-- Seed: 8757112134319189242,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity obp is
  port (fopu : buffer std_logic_vector(1 downto 2); nfkagz : out bit_vector(2 downto 2); zzhhb : linkage integer; qnlbiiu : buffer boolean);
end obp;

architecture hvbs of obp is
  
begin
  -- Single-driven assignments
  qnlbiiu <= TRUE;
  nfkagz <= (others => '1');
  
  -- Multi-driven assignments
  fopu <= fopu;
  fopu <= fopu;
end hvbs;

library ieee;
use ieee.std_logic_1164.all;

entity vvyvehh is
  port (oxwpxn : buffer real; lgwy : linkage std_logic_vector(3 downto 4); ufp : linkage character; msxdlci : inout severity_level);
end vvyvehh;

library ieee;
use ieee.std_logic_1164.all;

architecture u of vvyvehh is
  signal nd : boolean;
  signal igjlazie : integer;
  signal jbyo : bit_vector(2 downto 2);
  signal zlgcy : std_logic_vector(1 downto 2);
  signal qqstrhmteu : boolean;
  signal a : integer;
  signal mzoowmoa : bit_vector(2 downto 2);
  signal gyewguvylf : std_logic_vector(1 downto 2);
  signal iqsakcah : boolean;
  signal k : integer;
  signal yohsip : bit_vector(2 downto 2);
  signal ja : std_logic_vector(1 downto 2);
  signal et : boolean;
  signal ka : integer;
  signal t : bit_vector(2 downto 2);
  signal jffvbp : std_logic_vector(1 downto 2);
begin
  zw : entity work.obp
    port map (fopu => jffvbp, nfkagz => t, zzhhb => ka, qnlbiiu => et);
  vd : entity work.obp
    port map (fopu => ja, nfkagz => yohsip, zzhhb => k, qnlbiiu => iqsakcah);
  edq : entity work.obp
    port map (fopu => gyewguvylf, nfkagz => mzoowmoa, zzhhb => a, qnlbiiu => qqstrhmteu);
  muwcnsyrq : entity work.obp
    port map (fopu => zlgcy, nfkagz => jbyo, zzhhb => igjlazie, qnlbiiu => nd);
  
  -- Single-driven assignments
  msxdlci <= NOTE;
  oxwpxn <= 2#0100.0000#;
  
  -- Multi-driven assignments
  zlgcy <= (others => '0');
end u;

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (yybe : buffer std_logic; z : inout std_logic_vector(3 downto 4); flffxh : out std_logic);
end g;

architecture ndttzt of g is
  signal vvtigxnpfq : severity_level;
  signal xjzjmqujqs : character;
  signal bhvzb : real;
  signal ixwww : boolean;
  signal bvo : integer;
  signal cj : bit_vector(2 downto 2);
begin
  rhuasvbdcf : entity work.obp
    port map (fopu => z, nfkagz => cj, zzhhb => bvo, qnlbiiu => ixwww);
  wpbvs : entity work.vvyvehh
    port map (oxwpxn => bhvzb, lgwy => z, ufp => xjzjmqujqs, msxdlci => vvtigxnpfq);
  
  -- Multi-driven assignments
  flffxh <= 'H';
  yybe <= flffxh;
  yybe <= '0';
  flffxh <= flffxh;
end ndttzt;



-- Seed after: 17100315040348212636,1112937151005418631

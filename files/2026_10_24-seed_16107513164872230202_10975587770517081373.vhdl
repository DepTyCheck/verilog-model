-- Seed: 16107513164872230202,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity zjzp is
  port (qhqjj : buffer integer; h : linkage integer; jt : out time; zaupxlmcju : inout std_logic);
end zjzp;

architecture ppdrzrvu of zjzp is
  
begin
  -- Multi-driven assignments
  zaupxlmcju <= 'Z';
end ppdrzrvu;

entity dhottbvyw is
  port (uyasijgq : in time; smldu : in real; ulsdxitg : in bit_vector(0 to 2); zmhzunq : inout character);
end dhottbvyw;

library ieee;
use ieee.std_logic_1164.all;

architecture esshkweofy of dhottbvyw is
  signal olbzbeauc : std_logic;
  signal cqhwuxqb : time;
  signal vsd : integer;
  signal ofnsabd : integer;
  signal ncsip : std_logic;
  signal jk : time;
  signal rlt : integer;
  signal c : integer;
  signal accogdltxp : time;
  signal slha : integer;
  signal rqebz : integer;
  signal htm : std_logic;
  signal g : time;
  signal quzrrbmsmz : integer;
  signal micjljo : integer;
begin
  huplpo : entity work.zjzp
    port map (qhqjj => micjljo, h => quzrrbmsmz, jt => g, zaupxlmcju => htm);
  qp : entity work.zjzp
    port map (qhqjj => rqebz, h => slha, jt => accogdltxp, zaupxlmcju => htm);
  va : entity work.zjzp
    port map (qhqjj => c, h => rlt, jt => jk, zaupxlmcju => ncsip);
  ulz : entity work.zjzp
    port map (qhqjj => ofnsabd, h => vsd, jt => cqhwuxqb, zaupxlmcju => olbzbeauc);
end esshkweofy;

entity kueknv is
  port (tmwhus : inout integer; fx : buffer boolean_vector(4 downto 1));
end kueknv;

library ieee;
use ieee.std_logic_1164.all;

architecture mexamuxym of kueknv is
  signal uxc : time;
  signal iltgvll : integer;
  signal ov : std_logic;
  signal wmzywr : time;
  signal lo : integer;
  signal tbyaopgda : integer;
begin
  bpvjmdqo : entity work.zjzp
    port map (qhqjj => tbyaopgda, h => lo, jt => wmzywr, zaupxlmcju => ov);
  dz : entity work.zjzp
    port map (qhqjj => iltgvll, h => tmwhus, jt => uxc, zaupxlmcju => ov);
  
  -- Single-driven assignments
  fx <= fx;
  
  -- Multi-driven assignments
  ov <= 'L';
end mexamuxym;

entity eaczycsbms is
  port (oyccvnjcge : in time);
end eaczycsbms;

library ieee;
use ieee.std_logic_1164.all;

architecture wyerdaek of eaczycsbms is
  signal jbzskuhkhk : std_logic;
  signal hvfmkygmx : time;
  signal ah : integer;
  signal ub : integer;
  signal k : character;
  signal iumqc : bit_vector(0 to 2);
  signal dqahfnu : real;
begin
  ucomlls : entity work.dhottbvyw
    port map (uyasijgq => oyccvnjcge, smldu => dqahfnu, ulsdxitg => iumqc, zmhzunq => k);
  v : entity work.zjzp
    port map (qhqjj => ub, h => ah, jt => hvfmkygmx, zaupxlmcju => jbzskuhkhk);
  
  -- Single-driven assignments
  dqahfnu <= 2#1_1_0.1_0_1_0_0#;
  iumqc <= ('0', '1', '1');
  
  -- Multi-driven assignments
  jbzskuhkhk <= '0';
  jbzskuhkhk <= 'U';
  jbzskuhkhk <= jbzskuhkhk;
  jbzskuhkhk <= jbzskuhkhk;
end wyerdaek;



-- Seed after: 17390316849058259968,10975587770517081373

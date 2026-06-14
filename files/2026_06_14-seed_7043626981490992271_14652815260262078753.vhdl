-- Seed: 7043626981490992271,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity ntebaro is
  port (lpgzkktw : linkage real; skdyqsudhs : linkage std_logic);
end ntebaro;

architecture ggm of ntebaro is
  
begin
  
end ggm;

entity yolegwb is
  port (cy : buffer bit; cmbqomqxz : linkage bit_vector(0 to 2); henxozk : out time_vector(0 downto 0));
end yolegwb;

library ieee;
use ieee.std_logic_1164.all;

architecture buunkxtg of yolegwb is
  signal gdza : real;
  signal lcdgb : std_logic;
  signal sxsfybgn : real;
begin
  z : entity work.ntebaro
    port map (lpgzkktw => sxsfybgn, skdyqsudhs => lcdgb);
  dbon : entity work.ntebaro
    port map (lpgzkktw => gdza, skdyqsudhs => lcdgb);
  
  -- Single-driven assignments
  henxozk <= (others => 8#2_4_2# ps);
  cy <= '1';
  
  -- Multi-driven assignments
  lcdgb <= 'X';
  lcdgb <= '0';
  lcdgb <= 'L';
end buunkxtg;

library ieee;
use ieee.std_logic_1164.all;

entity esnkjevpt is
  port (vdcknf : linkage boolean; hxuk : in std_logic_vector(0 to 1));
end esnkjevpt;

architecture ql of esnkjevpt is
  signal hkn : time_vector(0 downto 0);
  signal laawcptqxt : bit_vector(0 to 2);
  signal wnulobiyfa : bit;
  signal fqheorr : time_vector(0 downto 0);
  signal drrdsc : bit_vector(0 to 2);
  signal zwfxmb : bit;
begin
  huhoaj : entity work.yolegwb
    port map (cy => zwfxmb, cmbqomqxz => drrdsc, henxozk => fqheorr);
  awf : entity work.yolegwb
    port map (cy => wnulobiyfa, cmbqomqxz => laawcptqxt, henxozk => hkn);
end ql;

entity s is
  port (taxguse : in integer);
end s;

architecture v of s is
  signal fbsje : time_vector(0 downto 0);
  signal auygaea : bit_vector(0 to 2);
  signal yufon : bit;
  signal evjgz : time_vector(0 downto 0);
  signal ost : bit_vector(0 to 2);
  signal iqhcfbljfd : bit;
  signal bud : time_vector(0 downto 0);
  signal x : bit_vector(0 to 2);
  signal immxy : bit;
  signal vxnesh : time_vector(0 downto 0);
  signal udmwtir : bit_vector(0 to 2);
  signal izanrj : bit;
begin
  b : entity work.yolegwb
    port map (cy => izanrj, cmbqomqxz => udmwtir, henxozk => vxnesh);
  ensjurxdzn : entity work.yolegwb
    port map (cy => immxy, cmbqomqxz => x, henxozk => bud);
  orf : entity work.yolegwb
    port map (cy => iqhcfbljfd, cmbqomqxz => ost, henxozk => evjgz);
  fbute : entity work.yolegwb
    port map (cy => yufon, cmbqomqxz => auygaea, henxozk => fbsje);
end v;



-- Seed after: 11145211149162827265,14652815260262078753

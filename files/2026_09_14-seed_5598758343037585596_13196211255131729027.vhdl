-- Seed: 5598758343037585596,13196211255131729027

entity lw is
  port (sqvp : in integer; dptmb : linkage real; rmbvfypl : inout bit_vector(2 downto 4); djdqchks : buffer time_vector(4 to 0));
end lw;

architecture qsjwo of lw is
  
begin
  
end qsjwo;

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (w : out severity_level; kvb : linkage std_logic_vector(1 to 2); avxopdc : buffer integer; fsszp : buffer integer);
end z;

architecture prggyfbna of z is
  signal sfzmliytq : time_vector(4 to 0);
  signal c : bit_vector(2 downto 4);
  signal ntvs : real;
  signal lsq : time_vector(4 to 0);
  signal skviasgrw : bit_vector(2 downto 4);
  signal kfrtqm : real;
  signal btqz : integer;
  signal dawuii : time_vector(4 to 0);
  signal rihpubrury : bit_vector(2 downto 4);
  signal fgwspaoy : real;
  signal t : integer;
begin
  tjhvqk : entity work.lw
    port map (sqvp => t, dptmb => fgwspaoy, rmbvfypl => rihpubrury, djdqchks => dawuii);
  qbgudx : entity work.lw
    port map (sqvp => btqz, dptmb => kfrtqm, rmbvfypl => skviasgrw, djdqchks => lsq);
  kdhokmmkpc : entity work.lw
    port map (sqvp => fsszp, dptmb => ntvs, rmbvfypl => c, djdqchks => sfzmliytq);
end prggyfbna;

entity nbrc is
  port (wejstntz : inout integer);
end nbrc;

architecture jsvrlar of nbrc is
  signal avyawoik : time_vector(4 to 0);
  signal khqkbmf : bit_vector(2 downto 4);
  signal cirqo : real;
  signal iiew : time_vector(4 to 0);
  signal xii : bit_vector(2 downto 4);
  signal jmadnrere : real;
begin
  lxdeett : entity work.lw
    port map (sqvp => wejstntz, dptmb => jmadnrere, rmbvfypl => xii, djdqchks => iiew);
  wzd : entity work.lw
    port map (sqvp => wejstntz, dptmb => cirqo, rmbvfypl => khqkbmf, djdqchks => avyawoik);
  
  -- Single-driven assignments
  wejstntz <= 2#0#;
end jsvrlar;

library ieee;
use ieee.std_logic_1164.all;

entity waoykqu is
  port (bdonk : buffer bit; ryxvvgwwl : out severity_level; mbqzkqfc : in time; i : buffer std_logic_vector(1 downto 4));
end waoykqu;

library ieee;
use ieee.std_logic_1164.all;

architecture vuar of waoykqu is
  signal vhccss : time_vector(4 to 0);
  signal uvobrmlfzi : bit_vector(2 downto 4);
  signal li : real;
  signal unsw : integer;
  signal ud : integer;
  signal hzzvpjbtx : integer;
  signal v : std_logic_vector(1 to 2);
begin
  fqrwfq : entity work.z
    port map (w => ryxvvgwwl, kvb => v, avxopdc => hzzvpjbtx, fsszp => ud);
  jgpqriyf : entity work.lw
    port map (sqvp => unsw, dptmb => li, rmbvfypl => uvobrmlfzi, djdqchks => vhccss);
  
  -- Single-driven assignments
  bdonk <= '1';
  unsw <= hzzvpjbtx;
  
  -- Multi-driven assignments
  i <= "";
  v <= "L1";
end vuar;



-- Seed after: 444101136634849827,13196211255131729027

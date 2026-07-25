-- Seed: 16090762426636105613,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity qwtfq is
  port (sznb : buffer integer; ibpc : inout std_logic_vector(3 to 4));
end qwtfq;

architecture wm of qwtfq is
  
begin
  -- Single-driven assignments
  sznb <= 4;
  
  -- Multi-driven assignments
  ibpc <= "X0";
  ibpc <= ('X', '-');
end wm;

library ieee;
use ieee.std_logic_1164.all;

entity allgj is
  port (sixlbk : buffer std_logic_vector(3 downto 0); mwospphqan : in time; rvi : in real);
end allgj;

library ieee;
use ieee.std_logic_1164.all;

architecture kgxvtz of allgj is
  signal kzythhub : std_logic_vector(3 to 4);
  signal xxznociumk : integer;
  signal varqrsddtq : std_logic_vector(3 to 4);
  signal gbgt : integer;
begin
  kq : entity work.qwtfq
    port map (sznb => gbgt, ibpc => varqrsddtq);
  h : entity work.qwtfq
    port map (sznb => xxznociumk, ibpc => kzythhub);
  
  -- Multi-driven assignments
  sixlbk <= ('L', 'L', 'W', '1');
end kgxvtz;

entity uhebj is
  port (v : out bit; vns : out time_vector(2 to 4); rnryy : in time);
end uhebj;

library ieee;
use ieee.std_logic_1164.all;

architecture rcm of uhebj is
  signal ofaug : std_logic_vector(3 to 4);
  signal okc : integer;
  signal a : std_logic_vector(3 to 4);
  signal jntxsbkih : integer;
  signal ldtzplh : integer;
  signal qha : std_logic_vector(3 to 4);
  signal jzt : integer;
begin
  rcs : entity work.qwtfq
    port map (sznb => jzt, ibpc => qha);
  ehepzaajqh : entity work.qwtfq
    port map (sznb => ldtzplh, ibpc => qha);
  mfthwvp : entity work.qwtfq
    port map (sznb => jntxsbkih, ibpc => a);
  dvbosjasm : entity work.qwtfq
    port map (sznb => okc, ibpc => ofaug);
  
  -- Single-driven assignments
  vns <= (2#0# us, 16#E_2_1_7_F.62507# ps, 1_0_1_1_1.3_4_4 fs);
  v <= '1';
  
  -- Multi-driven assignments
  a <= ('X', '0');
  qha <= ('-', 'W');
  qha <= "XW";
end rcm;

library ieee;
use ieee.std_logic_1164.all;

entity drmgrwmec is
  port (dh : buffer std_logic; hdatjsoqi : in std_logic; prbgzpclv : out integer);
end drmgrwmec;

library ieee;
use ieee.std_logic_1164.all;

architecture pevwfaep of drmgrwmec is
  signal x : time;
  signal xqsenl : time_vector(2 to 4);
  signal xfag : bit;
  signal xwtfiafjel : time;
  signal gwmoryj : time_vector(2 to 4);
  signal wvzmgrusd : bit;
  signal m : std_logic_vector(3 to 4);
  signal gbrhs : integer;
begin
  rvlxt : entity work.qwtfq
    port map (sznb => gbrhs, ibpc => m);
  fuwimz : entity work.uhebj
    port map (v => wvzmgrusd, vns => gwmoryj, rnryy => xwtfiafjel);
  n : entity work.uhebj
    port map (v => xfag, vns => xqsenl, rnryy => x);
  
  -- Single-driven assignments
  xwtfiafjel <= 0_4_0_3_3.4_3_0_4_0 us;
  x <= 34103 fs;
  prbgzpclv <= prbgzpclv;
  
  -- Multi-driven assignments
  dh <= '0';
end pevwfaep;



-- Seed after: 13293114376937182175,5306691039457971049

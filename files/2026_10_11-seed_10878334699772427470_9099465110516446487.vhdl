-- Seed: 10878334699772427470,9099465110516446487

entity saakdeb is
  port (xski : in time; guaqh : buffer real);
end saakdeb;

architecture uzgq of saakdeb is
  
begin
  -- Single-driven assignments
  guaqh <= guaqh;
end uzgq;

library ieee;
use ieee.std_logic_1164.all;

entity lrerf is
  port (ao : in std_logic_vector(1 to 0); xduoqew : buffer integer; q : buffer time; p : out real);
end lrerf;

architecture sopettwho of lrerf is
  signal sjlzan : time;
begin
  mumpkm : entity work.saakdeb
    port map (xski => sjlzan, guaqh => p);
  
  -- Single-driven assignments
  xduoqew <= 2#111#;
  sjlzan <= q;
  q <= 3_2 fs;
end sopettwho;

library ieee;
use ieee.std_logic_1164.all;

entity fwzsy is
  port (bpnrorlz : inout integer; ktntg : linkage integer; ehdw : out std_logic_vector(0 downto 2));
end fwzsy;

library ieee;
use ieee.std_logic_1164.all;

architecture vlfssn of fwzsy is
  signal oz : real;
  signal aqmwa : std_logic_vector(1 to 0);
  signal eigslmbe : real;
  signal bvylyz : time;
  signal fwqzuhhlb : integer;
  signal bnqz : real;
  signal gbvzvhsmb : time;
  signal snzbvrd : integer;
  signal wak : std_logic_vector(1 to 0);
  signal mbvrphi : real;
  signal pb : time;
begin
  rxxugred : entity work.saakdeb
    port map (xski => pb, guaqh => mbvrphi);
  akrgab : entity work.lrerf
    port map (ao => wak, xduoqew => snzbvrd, q => gbvzvhsmb, p => bnqz);
  wiaaxvhhrp : entity work.lrerf
    port map (ao => ehdw, xduoqew => fwqzuhhlb, q => bvylyz, p => eigslmbe);
  j : entity work.lrerf
    port map (ao => aqmwa, xduoqew => bpnrorlz, q => pb, p => oz);
end vlfssn;



-- Seed after: 8277030138941765101,9099465110516446487

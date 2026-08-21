-- Seed: 4428387983669261223,16188444798499499427

entity cw is
  port (xlsjkvqq : buffer time; rrjnikvyf : buffer time_vector(4 downto 4); bydtzkurlz : linkage real);
end cw;

architecture wdrn of cw is
  
begin
  -- Single-driven assignments
  rrjnikvyf <= (others => 8#3_7.1_4_2# ns);
  xlsjkvqq <= 0 sec;
end wdrn;

library ieee;
use ieee.std_logic_1164.all;

entity gixqn is
  port (uquikd : linkage std_logic_vector(3 to 0); p : inout time; gjqmu : linkage std_logic; hqzoxqyh : out std_logic);
end gixqn;

architecture gwnunzwbw of gixqn is
  signal ncxo : real;
  signal qdtleuhe : time_vector(4 downto 4);
  signal s : time;
  signal btxaarzicm : real;
  signal ngsnhmbgdj : time_vector(4 downto 4);
  signal zf : time;
  signal gocm : real;
  signal pn : time_vector(4 downto 4);
  signal zxs : time;
  signal egqairw : real;
  signal mwhkdeln : time_vector(4 downto 4);
  signal kswf : time;
begin
  uqmkvb : entity work.cw
    port map (xlsjkvqq => kswf, rrjnikvyf => mwhkdeln, bydtzkurlz => egqairw);
  wsu : entity work.cw
    port map (xlsjkvqq => zxs, rrjnikvyf => pn, bydtzkurlz => gocm);
  eiksnvmgq : entity work.cw
    port map (xlsjkvqq => zf, rrjnikvyf => ngsnhmbgdj, bydtzkurlz => btxaarzicm);
  urfbu : entity work.cw
    port map (xlsjkvqq => s, rrjnikvyf => qdtleuhe, bydtzkurlz => ncxo);
  
  -- Single-driven assignments
  p <= s;
end gwnunzwbw;



-- Seed after: 11753381455930152931,16188444798499499427

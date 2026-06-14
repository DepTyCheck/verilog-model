-- Seed: 16736046567079051361,1641934135882347475

library ieee;
use ieee.std_logic_1164.all;

entity woc is
  port (wb : inout std_logic; imeodx : in integer; whcr : linkage integer_vector(2 downto 3); xpmjmzk : linkage real_vector(4 downto 2));
end woc;



architecture ihlcc of woc is
  
begin
  
end ihlcc;



entity gdxzrdf is
  port (vpxbpxued : out bit_vector(1 to 0); wurehui : linkage severity_level; aakycuw : linkage time);
end gdxzrdf;

library ieee;
use ieee.std_logic_1164.all;

architecture ksswj of gdxzrdf is
  signal pjjic : integer_vector(2 downto 3);
  signal vsijwwjnv : integer;
  signal lezwmajjwk : real_vector(4 downto 2);
  signal dxjotufh : integer_vector(2 downto 3);
  signal gzlqcxhw : integer;
  signal tqsig : std_logic;
begin
  pdmue : entity work.woc
    port map (wb => tqsig, imeodx => gzlqcxhw, whcr => dxjotufh, xpmjmzk => lezwmajjwk);
  egcq : entity work.woc
    port map (wb => tqsig, imeodx => vsijwwjnv, whcr => dxjotufh, xpmjmzk => lezwmajjwk);
  b : entity work.woc
    port map (wb => tqsig, imeodx => gzlqcxhw, whcr => dxjotufh, xpmjmzk => lezwmajjwk);
  euiefow : entity work.woc
    port map (wb => tqsig, imeodx => gzlqcxhw, whcr => pjjic, xpmjmzk => lezwmajjwk);
end ksswj;



-- Seed after: 7379975400839631393,1641934135882347475

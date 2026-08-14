-- Seed: 14673954126975617012,8437298063418820479

entity qqqpfrzasi is
  port (pjhcqaex : inout real_vector(2 to 3); yxarst : buffer time; gu : out real);
end qqqpfrzasi;

architecture mzpbmpdxci of qqqpfrzasi is
  
begin
  -- Single-driven assignments
  yxarst <= 2#1110# us;
  gu <= 1_0.4_1_3;
end mzpbmpdxci;

entity mdtj is
  port (qai : linkage string(2 downto 5); tqahzqkhnl : inout real; yhmkxdxigm : in real; sfenqtm : in time);
end mdtj;

architecture wpx of mdtj is
  signal wk : real;
  signal ln : time;
  signal hgj : real_vector(2 to 3);
begin
  qwiqfq : entity work.qqqpfrzasi
    port map (pjhcqaex => hgj, yxarst => ln, gu => wk);
  
  -- Single-driven assignments
  tqahzqkhnl <= wk;
end wpx;

entity vnw is
  port (opoktqx : buffer real);
end vnw;

architecture w of vnw is
  signal itcok : real;
  signal dwvcqa : time;
  signal fndz : real_vector(2 to 3);
  signal rh : time;
  signal lwrepmzgo : real_vector(2 to 3);
begin
  ertyiwkd : entity work.qqqpfrzasi
    port map (pjhcqaex => lwrepmzgo, yxarst => rh, gu => opoktqx);
  umwnczieoo : entity work.qqqpfrzasi
    port map (pjhcqaex => fndz, yxarst => dwvcqa, gu => itcok);
end w;

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (vunxuq : in time; fn : buffer std_logic; xjtnohxf : buffer integer_vector(3 downto 2));
end k;

architecture aqymkoemae of k is
  
begin
  -- Single-driven assignments
  xjtnohxf <= (2_3, 3110);
  
  -- Multi-driven assignments
  fn <= '1';
  fn <= 'U';
  fn <= 'Z';
  fn <= fn;
end aqymkoemae;



-- Seed after: 13158010391469678130,8437298063418820479

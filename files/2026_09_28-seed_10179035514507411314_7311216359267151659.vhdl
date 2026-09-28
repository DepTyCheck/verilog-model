-- Seed: 10179035514507411314,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity jnca is
  port (h : in integer; obkp : out real; duhlzwbalr : buffer std_logic_vector(4 to 1); qfzzxmm : out std_logic);
end jnca;

architecture cetytluuy of jnca is
  
begin
  -- Single-driven assignments
  obkp <= 21.44;
end cetytluuy;

library ieee;
use ieee.std_logic_1164.all;

entity ijghatoxu is
  port (twscnie : in std_logic);
end ijghatoxu;

library ieee;
use ieee.std_logic_1164.all;

architecture fipxcijl of ijghatoxu is
  signal ztqlqqnh : std_logic;
  signal okupxdamt : real;
  signal uwdurcnim : integer;
  signal kmmgiims : real;
  signal c : std_logic_vector(4 to 1);
  signal oye : real;
  signal uwtf : std_logic;
  signal revpyuazh : std_logic_vector(4 to 1);
  signal qzr : real;
  signal vndsya : integer;
begin
  reol : entity work.jnca
    port map (h => vndsya, obkp => qzr, duhlzwbalr => revpyuazh, qfzzxmm => uwtf);
  hfpiweqi : entity work.jnca
    port map (h => vndsya, obkp => oye, duhlzwbalr => c, qfzzxmm => uwtf);
  r : entity work.jnca
    port map (h => vndsya, obkp => kmmgiims, duhlzwbalr => revpyuazh, qfzzxmm => uwtf);
  ts : entity work.jnca
    port map (h => uwdurcnim, obkp => okupxdamt, duhlzwbalr => revpyuazh, qfzzxmm => ztqlqqnh);
  
  -- Single-driven assignments
  vndsya <= uwdurcnim;
  uwdurcnim <= vndsya;
end fipxcijl;

library ieee;
use ieee.std_logic_1164.all;

entity zcka is
  port (dfhws : in real; bw : out std_logic; svjhavre : in real);
end zcka;

library ieee;
use ieee.std_logic_1164.all;

architecture gjwriukca of zcka is
  signal dr : std_logic_vector(4 to 1);
  signal rfip : real;
  signal hotjluydzs : std_logic;
  signal huopfj : std_logic;
  signal scquthtkd : std_logic_vector(4 to 1);
  signal tmevbsd : real;
  signal c : integer;
begin
  xigxwfzuky : entity work.jnca
    port map (h => c, obkp => tmevbsd, duhlzwbalr => scquthtkd, qfzzxmm => huopfj);
  jyx : entity work.ijghatoxu
    port map (twscnie => hotjluydzs);
  xro : entity work.jnca
    port map (h => c, obkp => rfip, duhlzwbalr => dr, qfzzxmm => huopfj);
  
  -- Single-driven assignments
  c <= c;
  
  -- Multi-driven assignments
  scquthtkd <= (others => '0');
  bw <= 'Z';
  hotjluydzs <= 'W';
end gjwriukca;

entity brgtkzl is
  port (rrzaisspi : linkage integer; xdy : out time_vector(2 to 4); tofdgvzqmp : linkage integer; oabzlrlis : in boolean_vector(2 to 4));
end brgtkzl;

library ieee;
use ieee.std_logic_1164.all;

architecture qesjntpw of brgtkzl is
  signal ybnpwezk : real;
  signal dlvswuhss : integer;
  signal owjgmbtv : std_logic;
  signal eghc : std_logic_vector(4 to 1);
  signal vat : real;
  signal upybz : integer;
begin
  z : entity work.jnca
    port map (h => upybz, obkp => vat, duhlzwbalr => eghc, qfzzxmm => owjgmbtv);
  uavzrq : entity work.ijghatoxu
    port map (twscnie => owjgmbtv);
  eofvbgssv : entity work.jnca
    port map (h => dlvswuhss, obkp => ybnpwezk, duhlzwbalr => eghc, qfzzxmm => owjgmbtv);
  
  -- Single-driven assignments
  dlvswuhss <= 16#D7#;
  upybz <= upybz;
  xdy <= (20 fs, 4_0_1_4_0.40 ps, 2.4_1_1_2 ms);
end qesjntpw;



-- Seed after: 12057177771623782510,7311216359267151659

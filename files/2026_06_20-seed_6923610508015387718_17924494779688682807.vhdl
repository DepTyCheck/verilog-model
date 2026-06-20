-- Seed: 6923610508015387718,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity osr is
  port (ilnhywhprk : linkage time; sfyrdbw : in std_logic; wlzcelpbuz : linkage bit; mfpl : out time);
end osr;

architecture ubusxcndnu of osr is
  
begin
  -- Single-driven assignments
  mfpl <= 34 ms;
end ubusxcndnu;

entity vtocj is
  port (k : buffer bit_vector(0 downto 3));
end vtocj;

library ieee;
use ieee.std_logic_1164.all;

architecture qixtqaaui of vtocj is
  signal gsfho : time;
  signal vwbugk : bit;
  signal kerxv : time;
  signal nqneglkl : time;
  signal zdhgjjevf : bit;
  signal f : std_logic;
  signal yqt : time;
begin
  rqixqbs : entity work.osr
    port map (ilnhywhprk => yqt, sfyrdbw => f, wlzcelpbuz => zdhgjjevf, mfpl => nqneglkl);
  nysw : entity work.osr
    port map (ilnhywhprk => kerxv, sfyrdbw => f, wlzcelpbuz => vwbugk, mfpl => gsfho);
  
  -- Single-driven assignments
  k <= (others => '0');
  
  -- Multi-driven assignments
  f <= '1';
  f <= '1';
  f <= 'L';
  f <= '1';
end qixtqaaui;

entity gstl is
  port (rmsogkkq : out character; jzorw : inout real);
end gstl;

library ieee;
use ieee.std_logic_1164.all;

architecture aftmxd of gstl is
  signal pkc : time;
  signal kylnunbsqr : bit;
  signal qyl : std_logic;
  signal tm : time;
  signal ubolsr : time;
  signal izvqiwhkm : bit;
  signal k : std_logic;
  signal vfygstyt : time;
  signal tc : time;
  signal qizgngcc : bit;
  signal gdvarsu : std_logic;
  signal yctn : time;
begin
  ezfxyazcuq : entity work.osr
    port map (ilnhywhprk => yctn, sfyrdbw => gdvarsu, wlzcelpbuz => qizgngcc, mfpl => tc);
  tljewdptoo : entity work.osr
    port map (ilnhywhprk => vfygstyt, sfyrdbw => k, wlzcelpbuz => izvqiwhkm, mfpl => ubolsr);
  eqyiijwan : entity work.osr
    port map (ilnhywhprk => tm, sfyrdbw => qyl, wlzcelpbuz => kylnunbsqr, mfpl => pkc);
end aftmxd;

library ieee;
use ieee.std_logic_1164.all;

entity jtczewp is
  port (tpciyk : linkage std_logic_vector(0 to 2); wes : buffer real; jhmezwja : linkage std_logic);
end jtczewp;

library ieee;
use ieee.std_logic_1164.all;

architecture uewxzlpm of jtczewp is
  signal tzregkaf : bit_vector(0 downto 3);
  signal ejltftnw : time;
  signal egadjscvt : bit;
  signal pr : time;
  signal rs : time;
  signal ujsmzzxjmh : bit;
  signal vqcffnpw : std_logic;
  signal bast : time;
  signal ppdl : time;
  signal a : bit;
  signal unfp : std_logic;
  signal vvexomqy : time;
begin
  pqqa : entity work.osr
    port map (ilnhywhprk => vvexomqy, sfyrdbw => unfp, wlzcelpbuz => a, mfpl => ppdl);
  akfjxbv : entity work.osr
    port map (ilnhywhprk => bast, sfyrdbw => vqcffnpw, wlzcelpbuz => ujsmzzxjmh, mfpl => rs);
  bxnqjekxo : entity work.osr
    port map (ilnhywhprk => pr, sfyrdbw => vqcffnpw, wlzcelpbuz => egadjscvt, mfpl => ejltftnw);
  jx : entity work.vtocj
    port map (k => tzregkaf);
  
  -- Single-driven assignments
  wes <= 3_4_4.0_4;
  
  -- Multi-driven assignments
  unfp <= '0';
  unfp <= 'L';
  vqcffnpw <= 'Z';
end uewxzlpm;



-- Seed after: 2033963625252987276,17924494779688682807

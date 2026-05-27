-- Seed: 5076144233414490827,13854332967471039201

library ieee;
use ieee.std_logic_1164.all;

entity xir is
  port (viowad : linkage std_logic; vnnoubozu : out real; e : in std_logic_vector(0 downto 2); oqlev : linkage real);
end xir;



architecture gqgysfiw of xir is
  
begin
  
end gqgysfiw;

library ieee;
use ieee.std_logic_1164.all;

entity xdov is
  port (syuqv : in std_logic_vector(4 downto 4); qbtmwvgk : buffer bit; mno : linkage integer);
end xdov;

library ieee;
use ieee.std_logic_1164.all;

architecture uttvyovc of xdov is
  signal phtz : real;
  signal fmn : std_logic;
  signal eetera : real;
  signal uarguju : std_logic_vector(0 downto 2);
  signal ys : real;
  signal p : std_logic;
begin
  bewjfclxn : entity work.xir
    port map (viowad => p, vnnoubozu => ys, e => uarguju, oqlev => eetera);
  htnpshfys : entity work.xir
    port map (viowad => fmn, vnnoubozu => eetera, e => uarguju, oqlev => phtz);
end uttvyovc;



entity tig is
  port (zijxfqjn : linkage character);
end tig;

library ieee;
use ieee.std_logic_1164.all;

architecture lew of tig is
  signal lyua : integer;
  signal jps : bit;
  signal vw : std_logic_vector(4 downto 4);
  signal osxufg : std_logic;
  signal celgbwmf : real;
  signal juyyhnjvqb : std_logic_vector(0 downto 2);
  signal ohgyqz : real;
  signal oorhjxxwj : std_logic;
  signal dsfggrwbxs : integer;
  signal gvhzruoiyy : bit;
  signal bpargjb : std_logic_vector(4 downto 4);
begin
  catovti : entity work.xdov
    port map (syuqv => bpargjb, qbtmwvgk => gvhzruoiyy, mno => dsfggrwbxs);
  ghycg : entity work.xir
    port map (viowad => oorhjxxwj, vnnoubozu => ohgyqz, e => juyyhnjvqb, oqlev => celgbwmf);
  p : entity work.xir
    port map (viowad => osxufg, vnnoubozu => celgbwmf, e => juyyhnjvqb, oqlev => ohgyqz);
  rodokz : entity work.xdov
    port map (syuqv => vw, qbtmwvgk => jps, mno => lyua);
end lew;

library ieee;
use ieee.std_logic_1164.all;

entity nyqifja is
  port (xy : inout std_logic_vector(1 downto 2); cvmi : buffer real);
end nyqifja;

library ieee;
use ieee.std_logic_1164.all;

architecture sxeduvwa of nyqifja is
  signal cxpcgndo : character;
  signal tglk : std_logic;
begin
  xcr : entity work.xir
    port map (viowad => tglk, vnnoubozu => cvmi, e => xy, oqlev => cvmi);
  vaxeyq : entity work.tig
    port map (zijxfqjn => cxpcgndo);
end sxeduvwa;



-- Seed after: 10017387595797027283,13854332967471039201

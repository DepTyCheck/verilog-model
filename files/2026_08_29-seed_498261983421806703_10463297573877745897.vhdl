-- Seed: 498261983421806703,10463297573877745897

entity ejmc is
  port (zeor : out integer_vector(3 downto 0));
end ejmc;

architecture ej of ejmc is
  
begin
  -- Single-driven assignments
  zeor <= (431, 3_3_0_3_4, 16#8#, 2_3);
end ej;

library ieee;
use ieee.std_logic_1164.all;

entity ybjb is
  port (yzuqvh : buffer std_logic_vector(2 downto 4));
end ybjb;

architecture ltsgq of ybjb is
  signal qs : integer_vector(3 downto 0);
  signal wyiufhu : integer_vector(3 downto 0);
  signal fajzqnlqs : integer_vector(3 downto 0);
begin
  obs : entity work.ejmc
    port map (zeor => fajzqnlqs);
  yf : entity work.ejmc
    port map (zeor => wyiufhu);
  xtvs : entity work.ejmc
    port map (zeor => qs);
  
  -- Multi-driven assignments
  yzuqvh <= (others => '0');
end ltsgq;

entity qihhlxqnjg is
  port (dmptv : buffer integer; ztdnjth : inout integer; d : out integer);
end qihhlxqnjg;

architecture rtmuvroo of qihhlxqnjg is
  
begin
  -- Single-driven assignments
  d <= 0_3_0_3;
  dmptv <= 2#11100#;
  ztdnjth <= 21221;
end rtmuvroo;

library ieee;
use ieee.std_logic_1164.all;

entity kzcok is
  port (ccfkk : inout std_logic_vector(0 to 1); lkbd : linkage time_vector(0 to 1); lmmofnxut : in integer; mp : buffer std_logic);
end kzcok;

library ieee;
use ieee.std_logic_1164.all;

architecture n of kzcok is
  signal fehfhpydi : integer;
  signal lw : integer;
  signal fqygsmh : integer;
  signal zfch : integer_vector(3 downto 0);
  signal vfsx : std_logic_vector(2 downto 4);
begin
  gxx : entity work.ybjb
    port map (yzuqvh => vfsx);
  mttclarlwx : entity work.ejmc
    port map (zeor => zfch);
  hdzrpyd : entity work.qihhlxqnjg
    port map (dmptv => fqygsmh, ztdnjth => lw, d => fehfhpydi);
  
  -- Multi-driven assignments
  vfsx <= (others => '0');
end n;



-- Seed after: 3465760088554839517,10463297573877745897

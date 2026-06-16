-- Seed: 7959992664050464453,5472058987609252853

entity abc is
  port (vrno : buffer bit; pprunknuoc : out time; uzlczbcotf : inout real_vector(1 to 0));
end abc;

architecture bdzqf of abc is
  
begin
  -- Single-driven assignments
  uzlczbcotf <= (others => 0.0);
end bdzqf;

library ieee;
use ieee.std_logic_1164.all;

entity wrfk is
  port (juebzzp : linkage integer; ziqa : linkage time; cp : linkage std_logic_vector(0 downto 1); jhhngs : in boolean);
end wrfk;

architecture rwi of wrfk is
  signal jxeftdg : real_vector(1 to 0);
  signal v : time;
  signal cqfajjlsez : bit;
begin
  gkdkx : entity work.abc
    port map (vrno => cqfajjlsez, pprunknuoc => v, uzlczbcotf => jxeftdg);
end rwi;

entity kadtvin is
  port (tfvltnbm : in real; zjf : buffer real; bxsiz : buffer boolean);
end kadtvin;

library ieee;
use ieee.std_logic_1164.all;

architecture aaouyrhbv of kadtvin is
  signal ysbdtble : boolean;
  signal ycisklynp : std_logic_vector(0 downto 1);
  signal vthzq : time;
  signal zb : integer;
  signal dudyqnkb : real_vector(1 to 0);
  signal bvboomske : time;
  signal ogeooxvvq : bit;
  signal jvldcsq : real_vector(1 to 0);
  signal iezhwdkd : time;
  signal augvfrcnfq : bit;
  signal pjmam : boolean;
  signal iyur : std_logic_vector(0 downto 1);
  signal g : time;
  signal gfsjrcq : integer;
begin
  fpaced : entity work.wrfk
    port map (juebzzp => gfsjrcq, ziqa => g, cp => iyur, jhhngs => pjmam);
  rwfuvfzyft : entity work.abc
    port map (vrno => augvfrcnfq, pprunknuoc => iezhwdkd, uzlczbcotf => jvldcsq);
  hl : entity work.abc
    port map (vrno => ogeooxvvq, pprunknuoc => bvboomske, uzlczbcotf => dudyqnkb);
  uesrhtizc : entity work.wrfk
    port map (juebzzp => zb, ziqa => vthzq, cp => ycisklynp, jhhngs => ysbdtble);
  
  -- Multi-driven assignments
  ycisklynp <= (others => '0');
  iyur <= "";
  iyur <= "";
  iyur <= "";
end aaouyrhbv;

entity jcorga is
  port (kflc : out character; bz : linkage boolean; yfoawwqe : in time; nz : out time);
end jcorga;

architecture wtzi of jcorga is
  
begin
  -- Single-driven assignments
  nz <= 8#1_6_5_7# ns;
  kflc <= 'j';
end wtzi;



-- Seed after: 11140335920396840992,5472058987609252853

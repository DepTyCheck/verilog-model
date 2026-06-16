-- Seed: 10087314751270159261,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity ksypzxca is
  port (qhyrlldvy : linkage real; mjcnjn : inout std_logic; tggrdziwg : out real; lgpppeuniv : inout time_vector(2 downto 2));
end ksypzxca;

architecture monoo of ksypzxca is
  
begin
  -- Single-driven assignments
  lgpppeuniv <= (others => 1.0_0_0 ms);
  tggrdziwg <= 8#2.0446#;
  
  -- Multi-driven assignments
  mjcnjn <= '-';
  mjcnjn <= '0';
end monoo;

library ieee;
use ieee.std_logic_1164.all;

entity nesywu is
  port (yj : linkage std_logic_vector(0 downto 3));
end nesywu;

library ieee;
use ieee.std_logic_1164.all;

architecture waoliwqkya of nesywu is
  signal ddruocjh : time_vector(2 downto 2);
  signal coiptb : real;
  signal pcczgzi : real;
  signal anbspm : time_vector(2 downto 2);
  signal uvsnasxddq : real;
  signal tzgihfw : std_logic;
  signal kis : real;
  signal daqhf : time_vector(2 downto 2);
  signal lqiirpi : real;
  signal ickdjd : std_logic;
  signal sjyc : real;
begin
  zcvkeh : entity work.ksypzxca
    port map (qhyrlldvy => sjyc, mjcnjn => ickdjd, tggrdziwg => lqiirpi, lgpppeuniv => daqhf);
  hctxblbvp : entity work.ksypzxca
    port map (qhyrlldvy => kis, mjcnjn => tzgihfw, tggrdziwg => uvsnasxddq, lgpppeuniv => anbspm);
  lqkhgh : entity work.ksypzxca
    port map (qhyrlldvy => pcczgzi, mjcnjn => tzgihfw, tggrdziwg => coiptb, lgpppeuniv => ddruocjh);
end waoliwqkya;

entity kcktf is
  port (ndqk : in real; og : buffer real);
end kcktf;

library ieee;
use ieee.std_logic_1164.all;

architecture gxftctyhxa of kcktf is
  signal kwbukqxpz : std_logic_vector(0 downto 3);
begin
  qxfnyxiz : entity work.nesywu
    port map (yj => kwbukqxpz);
  
  -- Single-driven assignments
  og <= 2.2;
  
  -- Multi-driven assignments
  kwbukqxpz <= "";
  kwbukqxpz <= "";
  kwbukqxpz <= (others => '0');
  kwbukqxpz <= (others => '0');
end gxftctyhxa;

library ieee;
use ieee.std_logic_1164.all;

entity kc is
  port (eorhbbq : inout time; tnnyckckl : buffer time; vrq : inout real; azdjruxdxi : buffer std_logic_vector(3 to 0));
end kc;

library ieee;
use ieee.std_logic_1164.all;

architecture swysy of kc is
  signal dvt : time_vector(2 downto 2);
  signal rnrhup : real;
  signal zrxybvp : std_logic;
  signal ckhicdzz : time_vector(2 downto 2);
  signal qebmhqjik : real;
  signal kanhi : real;
  signal gnwyhynmgb : time_vector(2 downto 2);
  signal kliqbgrvpu : real;
  signal suhqms : std_logic;
  signal psemsttv : real;
begin
  wnhrs : entity work.ksypzxca
    port map (qhyrlldvy => psemsttv, mjcnjn => suhqms, tggrdziwg => kliqbgrvpu, lgpppeuniv => gnwyhynmgb);
  tfx : entity work.nesywu
    port map (yj => azdjruxdxi);
  aj : entity work.ksypzxca
    port map (qhyrlldvy => kanhi, mjcnjn => suhqms, tggrdziwg => qebmhqjik, lgpppeuniv => ckhicdzz);
  lxryfb : entity work.ksypzxca
    port map (qhyrlldvy => vrq, mjcnjn => zrxybvp, tggrdziwg => rnrhup, lgpppeuniv => dvt);
  
  -- Single-driven assignments
  tnnyckckl <= 1 hr;
  
  -- Multi-driven assignments
  azdjruxdxi <= "";
  suhqms <= 'W';
  azdjruxdxi <= "";
  zrxybvp <= '-';
end swysy;



-- Seed after: 4613088614250765791,5472058987609252853

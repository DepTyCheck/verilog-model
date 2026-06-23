-- Seed: 16040326720108172098,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity lritep is
  port (tgjb : linkage std_logic; qiov : in std_logic_vector(3 downto 4); gahdqcbod : out time; dmhkq : buffer std_logic);
end lritep;

architecture bcmqvrpywy of lritep is
  
begin
  -- Multi-driven assignments
  dmhkq <= 'X';
  dmhkq <= 'Z';
end bcmqvrpywy;

entity flj is
  port (grt : inout time);
end flj;

library ieee;
use ieee.std_logic_1164.all;

architecture hyrxyx of flj is
  signal rrwthursq : std_logic_vector(3 downto 4);
  signal aj : std_logic;
  signal rdzgrxq : std_logic;
  signal sjhbxjwwjp : time;
  signal vk : std_logic_vector(3 downto 4);
  signal hnyuen : std_logic;
  signal abmnnpv : time;
  signal nuiu : std_logic_vector(3 downto 4);
  signal pwjshaetap : std_logic;
  signal ol : std_logic;
  signal bn : time;
  signal iswor : std_logic_vector(3 downto 4);
  signal vms : std_logic;
begin
  lnqkgre : entity work.lritep
    port map (tgjb => vms, qiov => iswor, gahdqcbod => bn, dmhkq => ol);
  opxqpe : entity work.lritep
    port map (tgjb => pwjshaetap, qiov => nuiu, gahdqcbod => abmnnpv, dmhkq => hnyuen);
  gaafcsa : entity work.lritep
    port map (tgjb => hnyuen, qiov => vk, gahdqcbod => sjhbxjwwjp, dmhkq => rdzgrxq);
  lajt : entity work.lritep
    port map (tgjb => aj, qiov => rrwthursq, gahdqcbod => grt, dmhkq => vms);
  
  -- Multi-driven assignments
  vk <= "";
  vms <= '1';
  ol <= 'L';
end hyrxyx;

entity kxxgihadv is
  port (ivaeplltx : linkage time; abjntdgf : out severity_level; mz : linkage character; fhwdmbpt : inout real);
end kxxgihadv;

library ieee;
use ieee.std_logic_1164.all;

architecture qpgkxq of kxxgihadv is
  signal bop : time;
  signal llv : time;
  signal y : std_logic;
  signal dfmgyvk : time;
  signal vhzbvpflch : std_logic;
  signal oglohlfx : std_logic;
  signal zqcjkdaawz : time;
  signal drjagkmgq : std_logic_vector(3 downto 4);
  signal urkxwcl : std_logic;
begin
  dmw : entity work.lritep
    port map (tgjb => urkxwcl, qiov => drjagkmgq, gahdqcbod => zqcjkdaawz, dmhkq => oglohlfx);
  xcjmmmr : entity work.lritep
    port map (tgjb => vhzbvpflch, qiov => drjagkmgq, gahdqcbod => dfmgyvk, dmhkq => y);
  ewalujs : entity work.flj
    port map (grt => llv);
  pakloxxlm : entity work.flj
    port map (grt => bop);
  
  -- Single-driven assignments
  abjntdgf <= FAILURE;
  fhwdmbpt <= 2#11.1_1_0_0_1#;
  
  -- Multi-driven assignments
  oglohlfx <= '1';
  urkxwcl <= 'L';
end qpgkxq;

library ieee;
use ieee.std_logic_1164.all;

entity shzyunk is
  port (zl : in std_logic; apm : in integer_vector(3 to 0); hayaziap : buffer real; hasanbg : in std_logic_vector(1 downto 3));
end shzyunk;

library ieee;
use ieee.std_logic_1164.all;

architecture yshcny of shzyunk is
  signal omrgcmux : std_logic;
  signal qyoknopj : time;
  signal witifazbe : std_logic;
  signal mbgg : time;
  signal yrzr : std_logic;
  signal cfggvhpr : time;
  signal xykdsc : std_logic;
  signal acuihlguye : std_logic;
  signal xfedxvbv : time;
begin
  fgycfnnb : entity work.lritep
    port map (tgjb => zl, qiov => hasanbg, gahdqcbod => xfedxvbv, dmhkq => acuihlguye);
  tondlbuu : entity work.lritep
    port map (tgjb => xykdsc, qiov => hasanbg, gahdqcbod => cfggvhpr, dmhkq => acuihlguye);
  sodyaleklr : entity work.lritep
    port map (tgjb => yrzr, qiov => hasanbg, gahdqcbod => mbgg, dmhkq => witifazbe);
  r : entity work.lritep
    port map (tgjb => zl, qiov => hasanbg, gahdqcbod => qyoknopj, dmhkq => omrgcmux);
  
  -- Single-driven assignments
  hayaziap <= 16#7.D_9_6_D_6#;
end yshcny;



-- Seed after: 15797184716444974719,8421704836678237495

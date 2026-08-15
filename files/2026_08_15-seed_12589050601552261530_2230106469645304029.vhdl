-- Seed: 12589050601552261530,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity skd is
  port (nfgac : buffer real; bnjmlzl : inout std_logic; fwmr : inout std_logic_vector(1 to 4); zffjkzle : inout real);
end skd;

architecture vosaid of skd is
  
begin
  -- Single-driven assignments
  zffjkzle <= 2003.4314;
  nfgac <= zffjkzle;
end vosaid;

library ieee;
use ieee.std_logic_1164.all;

entity qz is
  port (kpoyqyokn : in std_logic_vector(0 downto 3); suupf : linkage std_logic_vector(3 to 2); beizmduc : out std_logic);
end qz;

library ieee;
use ieee.std_logic_1164.all;

architecture omylzfv of qz is
  signal arvbsnbazz : real;
  signal gdo : std_logic_vector(1 to 4);
  signal vfyzhjficj : real;
  signal aifjvvmi : real;
  signal doya : std_logic_vector(1 to 4);
  signal izfp : std_logic;
  signal ggg : real;
  signal sflv : real;
  signal jkev : std_logic_vector(1 to 4);
  signal qxkhwpjsw : std_logic;
  signal scik : real;
begin
  lsnc : entity work.skd
    port map (nfgac => scik, bnjmlzl => qxkhwpjsw, fwmr => jkev, zffjkzle => sflv);
  tekfydpwjt : entity work.skd
    port map (nfgac => ggg, bnjmlzl => izfp, fwmr => doya, zffjkzle => aifjvvmi);
  lxv : entity work.skd
    port map (nfgac => vfyzhjficj, bnjmlzl => beizmduc, fwmr => gdo, zffjkzle => arvbsnbazz);
  
  -- Multi-driven assignments
  izfp <= beizmduc;
  izfp <= beizmduc;
  doya <= "X0UZ";
  beizmduc <= beizmduc;
end omylzfv;

entity r is
  port (v : buffer time_vector(1 downto 0));
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture kgqpo of r is
  signal klv : real;
  signal zubkhfygcc : std_logic_vector(1 to 4);
  signal jvygtiy : std_logic;
  signal cekyufcv : real;
  signal jhvbb : real;
  signal e : std_logic_vector(1 to 4);
  signal vbupzwutcj : std_logic;
  signal uwgqbo : real;
  signal arcsoz : real;
  signal gxnfnzwvgy : std_logic_vector(1 to 4);
  signal kqfkvl : std_logic;
  signal qvyznxoe : real;
begin
  llvtbiisob : entity work.skd
    port map (nfgac => qvyznxoe, bnjmlzl => kqfkvl, fwmr => gxnfnzwvgy, zffjkzle => arcsoz);
  ubfbhrk : entity work.skd
    port map (nfgac => uwgqbo, bnjmlzl => vbupzwutcj, fwmr => e, zffjkzle => jhvbb);
  xrwvdi : entity work.skd
    port map (nfgac => cekyufcv, bnjmlzl => jvygtiy, fwmr => zubkhfygcc, zffjkzle => klv);
  
  -- Single-driven assignments
  v <= (02.3 fs, 1_4_4_4_2 us);
  
  -- Multi-driven assignments
  kqfkvl <= kqfkvl;
end kgqpo;



-- Seed after: 2853638092744185506,2230106469645304029

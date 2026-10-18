-- Seed: 12479004906009018129,14108379317686958507

entity rtckqxaag is
  port (rpvkzqmt : inout time_vector(4 downto 3); s : out boolean);
end rtckqxaag;

architecture bafjvcv of rtckqxaag is
  
begin
  -- Single-driven assignments
  s <= FALSE;
  rpvkzqmt <= (4.0_4_1_2_4 ns, 1 min);
end bafjvcv;

entity fle is
  port (rajeiemkgf : inout time; ueq : linkage integer; avnfhlcsd : inout real; tzhwbbf : out time);
end fle;

architecture eogzjee of fle is
  signal hnhpx : boolean;
  signal viah : time_vector(4 downto 3);
  signal yrhuu : boolean;
  signal raemulj : time_vector(4 downto 3);
  signal sejkduu : boolean;
  signal mgtbjv : time_vector(4 downto 3);
begin
  i : entity work.rtckqxaag
    port map (rpvkzqmt => mgtbjv, s => sejkduu);
  zj : entity work.rtckqxaag
    port map (rpvkzqmt => raemulj, s => yrhuu);
  b : entity work.rtckqxaag
    port map (rpvkzqmt => viah, s => hnhpx);
  
  -- Single-driven assignments
  avnfhlcsd <= 20040.0_1_0_2;
  tzhwbbf <= rajeiemkgf;
  rajeiemkgf <= tzhwbbf;
end eogzjee;

entity uyf is
  port (whipetbir : buffer integer; hwreu : out boolean; dq : buffer time);
end uyf;

architecture hpodmreuw of uyf is
  signal fkhjyox : boolean;
  signal zqwhxeqx : time_vector(4 downto 3);
  signal tvi : time;
  signal hpzrgxb : real;
  signal tbqztpx : time_vector(4 downto 3);
  signal hu : time;
  signal tbwip : real;
  signal gppylq : integer;
  signal rrrqupxybt : time;
begin
  mus : entity work.fle
    port map (rajeiemkgf => rrrqupxybt, ueq => gppylq, avnfhlcsd => tbwip, tzhwbbf => hu);
  qyomgks : entity work.rtckqxaag
    port map (rpvkzqmt => tbqztpx, s => hwreu);
  btjsigi : entity work.fle
    port map (rajeiemkgf => dq, ueq => whipetbir, avnfhlcsd => hpzrgxb, tzhwbbf => tvi);
  pvc : entity work.rtckqxaag
    port map (rpvkzqmt => zqwhxeqx, s => fkhjyox);
end hpodmreuw;



-- Seed after: 208928023536491325,14108379317686958507

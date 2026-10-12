-- Seed: 6728403630276888768,6869379746012019351

entity kzva is
  port (lyvhqt : inout integer; ecuouisjw : inout integer);
end kzva;

architecture jqo of kzva is
  
begin
  -- Single-driven assignments
  ecuouisjw <= 2#0_1_1#;
  lyvhqt <= ecuouisjw;
end jqo;

entity tpg is
  port (yjo : linkage real_vector(0 to 3));
end tpg;

architecture adswwhfs of tpg is
  signal q : integer;
  signal bjuza : integer;
  signal ukksh : integer;
  signal wzsn : integer;
begin
  mmxp : entity work.kzva
    port map (lyvhqt => wzsn, ecuouisjw => ukksh);
  pslcyyqdro : entity work.kzva
    port map (lyvhqt => bjuza, ecuouisjw => q);
end adswwhfs;

library ieee;
use ieee.std_logic_1164.all;

entity gjnb is
  port (haj : buffer boolean_vector(4 to 3); caqzhanu : linkage std_logic_vector(1 downto 1));
end gjnb;

architecture jbvqp of gjnb is
  signal efc : integer;
  signal loub : integer;
  signal tscpivk : integer;
  signal ljvissjxs : integer;
  signal ffxwklsa : real_vector(0 to 3);
begin
  e : entity work.tpg
    port map (yjo => ffxwklsa);
  hpsrvl : entity work.kzva
    port map (lyvhqt => ljvissjxs, ecuouisjw => tscpivk);
  i : entity work.kzva
    port map (lyvhqt => loub, ecuouisjw => efc);
  
  -- Single-driven assignments
  haj <= haj;
end jbvqp;

entity qryb is
  port (mmz : linkage integer_vector(0 downto 4); pyqkslbmqg : out time; qtuydk : in integer);
end qryb;

library ieee;
use ieee.std_logic_1164.all;

architecture tbehne of qryb is
  signal nugbopkbqa : integer;
  signal usyztbw : integer;
  signal fnrzobvb : boolean_vector(4 to 3);
  signal rw : std_logic_vector(1 downto 1);
  signal upagrwdvz : boolean_vector(4 to 3);
  signal gdohbv : integer;
  signal ursqc : integer;
begin
  qensat : entity work.kzva
    port map (lyvhqt => ursqc, ecuouisjw => gdohbv);
  rt : entity work.gjnb
    port map (haj => upagrwdvz, caqzhanu => rw);
  q : entity work.gjnb
    port map (haj => fnrzobvb, caqzhanu => rw);
  bh : entity work.kzva
    port map (lyvhqt => usyztbw, ecuouisjw => nugbopkbqa);
  
  -- Single-driven assignments
  pyqkslbmqg <= pyqkslbmqg;
  
  -- Multi-driven assignments
  rw <= rw;
  rw <= rw;
  rw <= "Z";
end tbehne;



-- Seed after: 10692470458566050927,6869379746012019351

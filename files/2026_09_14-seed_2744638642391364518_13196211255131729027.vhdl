-- Seed: 2744638642391364518,13196211255131729027

entity oduyl is
  port (uksvru : out real_vector(4 downto 0); txlzczu : out integer; k : inout severity_level; pgifypqynr : buffer time);
end oduyl;

architecture bvvjqqkxt of oduyl is
  
begin
  -- Single-driven assignments
  pgifypqynr <= 2 min;
  uksvru <= (2#111.0101#, 4_4_0_4.2_2_3_4, 8#7_6_0_7_1.02#, 2110.0, 23044.4_4_4_3_2);
  k <= k;
  txlzczu <= 8#6_1_3#;
end bvvjqqkxt;

entity lg is
  port (goa : buffer integer);
end lg;

architecture slbqk of lg is
  signal tpkbiszmo : time;
  signal xwnqhx : severity_level;
  signal zjujytknqr : real_vector(4 downto 0);
  signal wckxktsuql : time;
  signal lselbrcwvv : severity_level;
  signal xnknwj : integer;
  signal nuyte : real_vector(4 downto 0);
  signal nmxkk : time;
  signal ujfbzkbvit : severity_level;
  signal updzrxa : integer;
  signal zxsrqttjeg : real_vector(4 downto 0);
  signal wixi : time;
  signal dyd : severity_level;
  signal hlcnsib : integer;
  signal nrkgmmpzos : real_vector(4 downto 0);
begin
  bxjw : entity work.oduyl
    port map (uksvru => nrkgmmpzos, txlzczu => hlcnsib, k => dyd, pgifypqynr => wixi);
  egkd : entity work.oduyl
    port map (uksvru => zxsrqttjeg, txlzczu => updzrxa, k => ujfbzkbvit, pgifypqynr => nmxkk);
  yaub : entity work.oduyl
    port map (uksvru => nuyte, txlzczu => xnknwj, k => lselbrcwvv, pgifypqynr => wckxktsuql);
  efx : entity work.oduyl
    port map (uksvru => zjujytknqr, txlzczu => goa, k => xwnqhx, pgifypqynr => tpkbiszmo);
end slbqk;

entity chl is
  port (ieecajpjv : inout integer; ihxjt : out integer; wvpujecw : in real);
end chl;

architecture tapquli of chl is
  
begin
  ayyklaha : entity work.lg
    port map (goa => ihxjt);
  
  -- Single-driven assignments
  ieecajpjv <= ihxjt;
end tapquli;



-- Seed after: 2217092484309771033,13196211255131729027

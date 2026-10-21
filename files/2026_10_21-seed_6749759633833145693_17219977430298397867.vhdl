-- Seed: 6749759633833145693,17219977430298397867

entity iay is
  port (pboky : out character; it : buffer severity_level; ssontm : linkage time_vector(4 downto 0); so : in real);
end iay;

architecture j of iay is
  
begin
  -- Single-driven assignments
  it <= NOTE;
  pboky <= 's';
end j;

entity kgggm is
  port (cztqwouja : inout time; hryvaa : in real; z : buffer integer_vector(3 to 4); ogzbd : out boolean);
end kgggm;

architecture nocyuyjie of kgggm is
  signal ojxijiqmf : time_vector(4 downto 0);
  signal zorkae : severity_level;
  signal ksd : character;
  signal yfh : time_vector(4 downto 0);
  signal zrhizscd : severity_level;
  signal crszr : character;
  signal hrdxyuyssw : real;
  signal mfmlof : time_vector(4 downto 0);
  signal x : severity_level;
  signal bmyhmkg : character;
  signal inb : real;
  signal mshqtlpb : time_vector(4 downto 0);
  signal zx : severity_level;
  signal tddxshmdc : character;
begin
  nn : entity work.iay
    port map (pboky => tddxshmdc, it => zx, ssontm => mshqtlpb, so => inb);
  etfajvdg : entity work.iay
    port map (pboky => bmyhmkg, it => x, ssontm => mfmlof, so => hrdxyuyssw);
  pnlqehiv : entity work.iay
    port map (pboky => crszr, it => zrhizscd, ssontm => yfh, so => hrdxyuyssw);
  hbvzqqwngw : entity work.iay
    port map (pboky => ksd, it => zorkae, ssontm => ojxijiqmf, so => hryvaa);
  
  -- Single-driven assignments
  inb <= hryvaa;
  z <= z;
  ogzbd <= FALSE;
  hrdxyuyssw <= 2#1.0_1_1_1#;
end nocyuyjie;

entity qjhsrpx is
  port (zgwyp : inout time; z : out integer);
end qjhsrpx;

architecture ovicjtqx of qjhsrpx is
  signal zwv : boolean;
  signal wzjo : integer_vector(3 to 4);
  signal itntpqfk : time;
  signal prmhfcyf : boolean;
  signal lvcttfvxb : integer_vector(3 to 4);
  signal evgctzxe : real;
  signal kbwuaapaj : time_vector(4 downto 0);
  signal fhay : severity_level;
  signal q : character;
  signal tsxrcfwcd : boolean;
  signal ma : integer_vector(3 to 4);
  signal cagco : real;
  signal h : time;
begin
  ebof : entity work.kgggm
    port map (cztqwouja => h, hryvaa => cagco, z => ma, ogzbd => tsxrcfwcd);
  ef : entity work.iay
    port map (pboky => q, it => fhay, ssontm => kbwuaapaj, so => cagco);
  g : entity work.kgggm
    port map (cztqwouja => zgwyp, hryvaa => evgctzxe, z => lvcttfvxb, ogzbd => prmhfcyf);
  khygzz : entity work.kgggm
    port map (cztqwouja => itntpqfk, hryvaa => cagco, z => wzjo, ogzbd => zwv);
  
  -- Single-driven assignments
  z <= 4;
  evgctzxe <= 16#DBCD4.B5#;
  cagco <= cagco;
end ovicjtqx;



-- Seed after: 8148554661465220173,17219977430298397867

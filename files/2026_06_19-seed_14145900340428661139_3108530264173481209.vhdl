-- Seed: 14145900340428661139,3108530264173481209

entity vmhblax is
  port (owhilps : inout time_vector(3 downto 1); vhri : buffer time; qzgq : inout real);
end vmhblax;

architecture hsybzbyk of vmhblax is
  
begin
  -- Single-driven assignments
  vhri <= 8#0_0_1_3_0.4# us;
  qzgq <= 003.1;
  owhilps <= (1.1 fs, 2_4_0.0222 us, 2#1_0.1# fs);
end hsybzbyk;

entity uuwg is
  port (zzfwdufox : in time);
end uuwg;

architecture lfz of uuwg is
  
begin
  
end lfz;

entity hvekihuz is
  port (ps : linkage real);
end hvekihuz;

architecture uhpqj of hvekihuz is
  signal va : real;
  signal tiimycfj : time;
  signal orj : time_vector(3 downto 1);
  signal sg : real;
  signal wgazgj : time;
  signal wfhrp : time_vector(3 downto 1);
  signal qprpfi : real;
  signal icxfnp : time;
  signal rtlhgsu : time_vector(3 downto 1);
  signal edeuhv : real;
  signal ayhivvd : time;
  signal khfjpzew : time_vector(3 downto 1);
begin
  knucedf : entity work.vmhblax
    port map (owhilps => khfjpzew, vhri => ayhivvd, qzgq => edeuhv);
  o : entity work.vmhblax
    port map (owhilps => rtlhgsu, vhri => icxfnp, qzgq => qprpfi);
  k : entity work.vmhblax
    port map (owhilps => wfhrp, vhri => wgazgj, qzgq => sg);
  hnzaupjz : entity work.vmhblax
    port map (owhilps => orj, vhri => tiimycfj, qzgq => va);
end uhpqj;

entity kbsfr is
  port (eynctumi : buffer time_vector(2 to 4); rdahz : out character; da : linkage real);
end kbsfr;

architecture rcaaj of kbsfr is
  signal jdciejvvz : real;
  signal thh : time;
  signal pmqxzvadj : real;
  signal egovvor : time_vector(3 downto 1);
  signal umn : time;
  signal an : real;
  signal pjctb : time;
  signal qt : time_vector(3 downto 1);
begin
  zrp : entity work.vmhblax
    port map (owhilps => qt, vhri => pjctb, qzgq => an);
  pummwe : entity work.uuwg
    port map (zzfwdufox => umn);
  ku : entity work.vmhblax
    port map (owhilps => egovvor, vhri => umn, qzgq => pmqxzvadj);
  ikarjlpymu : entity work.vmhblax
    port map (owhilps => eynctumi, vhri => thh, qzgq => jdciejvvz);
end rcaaj;



-- Seed after: 9042055226665863852,3108530264173481209

-- Seed: 14097523932526343642,8891552411914730853

entity aibz is
  port (zj : in real; yhugtzcj : linkage real; hjv : buffer bit; vsj : in time_vector(0 downto 3));
end aibz;

architecture ipkljcft of aibz is
  
begin
  
end ipkljcft;

entity iamrx is
  port (hjlpfyl : buffer real; vpu : out integer);
end iamrx;

architecture llozevlp of iamrx is
  signal tyrgkqs : time_vector(0 downto 3);
  signal tzmnwxlem : bit;
  signal hmrhszd : real;
  signal gmohbm : real;
  signal qhoy : bit;
  signal nan : real;
  signal yhuecj : real;
  signal q : time_vector(0 downto 3);
  signal m : bit;
  signal oscnhvjcqd : real;
  signal kunio : time_vector(0 downto 3);
  signal sdasp : bit;
begin
  klhm : entity work.aibz
    port map (zj => hjlpfyl, yhugtzcj => hjlpfyl, hjv => sdasp, vsj => kunio);
  ejpgcnma : entity work.aibz
    port map (zj => oscnhvjcqd, yhugtzcj => oscnhvjcqd, hjv => m, vsj => q);
  cpuzg : entity work.aibz
    port map (zj => yhuecj, yhugtzcj => nan, hjv => qhoy, vsj => q);
  npd : entity work.aibz
    port map (zj => gmohbm, yhugtzcj => hmrhszd, hjv => tzmnwxlem, vsj => tyrgkqs);
end llozevlp;

entity kao is
  port (l : linkage real);
end kao;

architecture beunmxn of kao is
  signal bcth : integer;
  signal hr : real;
  signal sudamyie : time_vector(0 downto 3);
  signal bvxjzut : bit;
  signal srkougsp : real;
  signal rbun : real;
begin
  bzwvjra : entity work.aibz
    port map (zj => rbun, yhugtzcj => srkougsp, hjv => bvxjzut, vsj => sudamyie);
  qpxrzsn : entity work.iamrx
    port map (hjlpfyl => hr, vpu => bcth);
  
  -- Single-driven assignments
  rbun <= 2#110.1_1_1_0#;
  sudamyie <= sudamyie;
end beunmxn;



-- Seed after: 13446616006241952853,8891552411914730853

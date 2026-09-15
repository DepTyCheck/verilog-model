-- Seed: 2184650637375815563,13613332369802491303

entity lhcnszht is
  port (qboeczuum : buffer integer; qformzqu : inout bit);
end lhcnszht;

architecture pfn of lhcnszht is
  
begin
  -- Single-driven assignments
  qformzqu <= '1';
  qboeczuum <= qboeczuum;
end pfn;

entity yngcz is
  port (vknfj : linkage time_vector(4 downto 0));
end yngcz;

architecture r of yngcz is
  signal yjtosfxwi : bit;
  signal ia : integer;
  signal cdpwhhrt : bit;
  signal mpo : integer;
  signal ay : bit;
  signal sim : integer;
  signal hw : bit;
  signal whgcft : integer;
begin
  cy : entity work.lhcnszht
    port map (qboeczuum => whgcft, qformzqu => hw);
  ngeaskbj : entity work.lhcnszht
    port map (qboeczuum => sim, qformzqu => ay);
  nawoir : entity work.lhcnszht
    port map (qboeczuum => mpo, qformzqu => cdpwhhrt);
  bgd : entity work.lhcnszht
    port map (qboeczuum => ia, qformzqu => yjtosfxwi);
end r;

entity g is
  port (uxpb : out boolean_vector(2 downto 1));
end g;

architecture omorqj of g is
  signal lwoiuorlcj : bit;
  signal bu : integer;
  signal pa : bit;
  signal n : integer;
  signal svxwkqs : bit;
  signal itx : integer;
  signal ro : time_vector(4 downto 0);
begin
  qfpfhfop : entity work.yngcz
    port map (vknfj => ro);
  vqvlh : entity work.lhcnszht
    port map (qboeczuum => itx, qformzqu => svxwkqs);
  lkgae : entity work.lhcnszht
    port map (qboeczuum => n, qformzqu => pa);
  burv : entity work.lhcnszht
    port map (qboeczuum => bu, qformzqu => lwoiuorlcj);
  
  -- Single-driven assignments
  uxpb <= uxpb;
end omorqj;



-- Seed after: 17326938328497330374,13613332369802491303

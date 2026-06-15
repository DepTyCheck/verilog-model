-- Seed: 9014771371347583831,15300320181035395489

entity nhaobqm is
  port (hikgkyyz : buffer real; pf : linkage real);
end nhaobqm;

architecture t of nhaobqm is
  
begin
  -- Single-driven assignments
  hikgkyyz <= 0_1_4.0_3_4;
end t;

entity zhytevi is
  port (bslgwk : out time);
end zhytevi;

architecture qfhcibg of zhytevi is
  signal p : real;
  signal lxegchzdp : real;
  signal zohznvcm : real;
  signal x : real;
begin
  pavzndbqn : entity work.nhaobqm
    port map (hikgkyyz => x, pf => zohznvcm);
  jcxkmteq : entity work.nhaobqm
    port map (hikgkyyz => lxegchzdp, pf => p);
end qfhcibg;

entity gvpf is
  port (d : out bit; i : out integer);
end gvpf;

architecture dtumos of gvpf is
  signal vokynkneno : real;
  signal jncgc : real;
  signal bdkgzis : real;
  signal nfeuunnpgq : real;
  signal lfxhkn : real;
  signal g : real;
begin
  veukssmd : entity work.nhaobqm
    port map (hikgkyyz => g, pf => lfxhkn);
  suick : entity work.nhaobqm
    port map (hikgkyyz => nfeuunnpgq, pf => bdkgzis);
  iskm : entity work.nhaobqm
    port map (hikgkyyz => jncgc, pf => vokynkneno);
  
  -- Single-driven assignments
  i <= 16#57#;
  d <= '0';
end dtumos;

entity eodkpbyj is
  port (peszrgjnj : inout integer_vector(2 to 4));
end eodkpbyj;

architecture ucqvn of eodkpbyj is
  signal wlxbrgpd : integer;
  signal plwnvh : bit;
  signal di : real;
  signal nshn : real;
begin
  yailont : entity work.nhaobqm
    port map (hikgkyyz => nshn, pf => di);
  kd : entity work.gvpf
    port map (d => plwnvh, i => wlxbrgpd);
  
  -- Single-driven assignments
  peszrgjnj <= (0_0_0_3, 3_3_3_1_4, 4401);
end ucqvn;



-- Seed after: 18150397751612684786,15300320181035395489

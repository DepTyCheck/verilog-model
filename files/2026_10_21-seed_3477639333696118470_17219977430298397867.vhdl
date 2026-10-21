-- Seed: 3477639333696118470,17219977430298397867

entity fvyyhsi is
  port (mhgoomy : linkage time; jyjunjgk : inout integer; mp : out bit; zte : linkage real);
end fvyyhsi;

architecture dyzbwwf of fvyyhsi is
  
begin
  -- Single-driven assignments
  mp <= '1';
  jyjunjgk <= 8#3040#;
end dyzbwwf;

entity axhhc is
  port (gzhg : linkage boolean_vector(2 downto 1); pgyv : inout real);
end axhhc;

architecture tusrwizppn of axhhc is
  signal fbof : real;
  signal sucyqv : bit;
  signal sty : integer;
  signal cqq : time;
  signal xheobx : bit;
  signal cvw : integer;
  signal gyr : time;
begin
  dgwlu : entity work.fvyyhsi
    port map (mhgoomy => gyr, jyjunjgk => cvw, mp => xheobx, zte => pgyv);
  eycuxpswx : entity work.fvyyhsi
    port map (mhgoomy => cqq, jyjunjgk => sty, mp => sucyqv, zte => fbof);
end tusrwizppn;

entity gszazxmgms is
  port (yigfaqfuw : linkage integer; knasocmbp : buffer real_vector(0 to 1); et : linkage real);
end gszazxmgms;

architecture bnmhkw of gszazxmgms is
  signal qin : real;
  signal iubc : boolean_vector(2 downto 1);
  signal li : real;
  signal tzav : boolean_vector(2 downto 1);
  signal jwpgt : bit;
  signal rj : integer;
  signal fe : time;
begin
  p : entity work.fvyyhsi
    port map (mhgoomy => fe, jyjunjgk => rj, mp => jwpgt, zte => et);
  awx : entity work.axhhc
    port map (gzhg => tzav, pgyv => li);
  a : entity work.axhhc
    port map (gzhg => iubc, pgyv => qin);
  
  -- Single-driven assignments
  knasocmbp <= (2#1_1.1110#, 2#00010.0#);
end bnmhkw;



-- Seed after: 4062034755567776092,17219977430298397867

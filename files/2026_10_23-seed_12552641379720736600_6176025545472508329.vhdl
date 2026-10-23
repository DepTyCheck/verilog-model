-- Seed: 12552641379720736600,6176025545472508329

entity odmhitfsp is
  port (f : linkage bit_vector(2 to 0); zdmvku : buffer severity_level; zkfew : buffer real; uefu : inout integer);
end odmhitfsp;

architecture zvyjjcjnl of odmhitfsp is
  
begin
  
end zvyjjcjnl;

entity ssmzfmhkml is
  port (bebf : out time; erq : out bit; o : out severity_level; oshas : inout boolean_vector(1 downto 2));
end ssmzfmhkml;

architecture lqvmhjeyj of ssmzfmhkml is
  signal ujinvkzzf : integer;
  signal wxzrroya : real;
  signal balsz : bit_vector(2 to 0);
  signal pwetc : integer;
  signal spslnoequ : real;
  signal uwlhrasii : severity_level;
  signal xmvrmcyinl : bit_vector(2 to 0);
  signal jka : integer;
  signal inlrl : real;
  signal mwvo : severity_level;
  signal vg : bit_vector(2 to 0);
  signal nkqprqc : integer;
  signal rmem : real;
  signal k : severity_level;
  signal nmrwgtwuj : bit_vector(2 to 0);
begin
  iinfzq : entity work.odmhitfsp
    port map (f => nmrwgtwuj, zdmvku => k, zkfew => rmem, uefu => nkqprqc);
  fpo : entity work.odmhitfsp
    port map (f => vg, zdmvku => mwvo, zkfew => inlrl, uefu => jka);
  geqwhseag : entity work.odmhitfsp
    port map (f => xmvrmcyinl, zdmvku => uwlhrasii, zkfew => spslnoequ, uefu => pwetc);
  cnm : entity work.odmhitfsp
    port map (f => balsz, zdmvku => o, zkfew => wxzrroya, uefu => ujinvkzzf);
  
  -- Single-driven assignments
  bebf <= 2_3_1_3 ns;
  erq <= erq;
  oshas <= oshas;
end lqvmhjeyj;

entity he is
  port (sud : in time);
end he;

architecture hp of he is
  signal djsj : integer;
  signal vjl : real;
  signal qenzg : severity_level;
  signal ipvoa : bit_vector(2 to 0);
begin
  jxxgplm : entity work.odmhitfsp
    port map (f => ipvoa, zdmvku => qenzg, zkfew => vjl, uefu => djsj);
end hp;

entity qq is
  port (fpddbod : in real);
end qq;

architecture raeydxzp of qq is
  signal ehkxs : boolean_vector(1 downto 2);
  signal vwvz : severity_level;
  signal jijysqmjsy : bit;
  signal fo : time;
  signal peunsgc : integer;
  signal vm : real;
  signal eqnapf : severity_level;
  signal n : bit_vector(2 to 0);
  signal elwejkqi : integer;
  signal oysgltznh : real;
  signal qjkzhqosmw : severity_level;
  signal mixgyyky : bit_vector(2 to 0);
begin
  qtgzmblytt : entity work.odmhitfsp
    port map (f => mixgyyky, zdmvku => qjkzhqosmw, zkfew => oysgltznh, uefu => elwejkqi);
  ekkouyxw : entity work.odmhitfsp
    port map (f => n, zdmvku => eqnapf, zkfew => vm, uefu => peunsgc);
  irp : entity work.ssmzfmhkml
    port map (bebf => fo, erq => jijysqmjsy, o => vwvz, oshas => ehkxs);
end raeydxzp;



-- Seed after: 570629969824425998,6176025545472508329
